"""DuckDB schema and operations for SPOJ data."""

from __future__ import annotations

import os
from pathlib import Path
import duckdb

DATA_DIR = Path(os.environ.get("SPOJ_DATA_DIR", Path.home() / "data" / "spoj"))
MAIN_DB = DATA_DIR / "spoj.duckdb"
STATE_DB = DATA_DIR / "state.duckdb"
EXPORT_DIR = DATA_DIR / "problems"

SECTIONS = ["classical", "tutorial", "challenge", "partial"]


def _ensure_dirs() -> None:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    EXPORT_DIR.mkdir(parents=True, exist_ok=True)


def open_main() -> duckdb.DuckDBPyConnection:
    _ensure_dirs()
    con = duckdb.connect(str(MAIN_DB))
    _init_main(con)
    return con


def open_state() -> duckdb.DuckDBPyConnection:
    _ensure_dirs()
    con = duckdb.connect(str(STATE_DB))
    _init_state(con)
    return con


def _init_main(con: duckdb.DuckDBPyConnection) -> None:
    con.execute("""
        CREATE TABLE IF NOT EXISTS problems (
            slug        VARCHAR NOT NULL,
            section     VARCHAR NOT NULL,
            rank        INTEGER,
            name        VARCHAR,
            url         VARCHAR,
            users       INTEGER,
            accepted    FLOAT,
            fetched_at  TIMESTAMPTZ DEFAULT now(),
            PRIMARY KEY (slug, section)
        )
    """)
    con.execute("""
        CREATE TABLE IF NOT EXISTS problem_bodies (
            slug        VARCHAR PRIMARY KEY,
            url         VARCHAR,
            html        TEXT,
            markdown    TEXT,
            fetched_at  TIMESTAMPTZ DEFAULT now()
        )
    """)
    # Index for fast rank lookups
    con.execute("""
        CREATE INDEX IF NOT EXISTS idx_problems_section_rank
        ON problems (section, rank)
    """)


def _init_state(con: duckdb.DuckDBPyConnection) -> None:
    con.execute("""
        CREATE TABLE IF NOT EXISTS fetch_state (
            task        VARCHAR PRIMARY KEY,
            status      VARCHAR DEFAULT 'pending',
            attempts    INTEGER DEFAULT 0,
            updated_at  TIMESTAMPTZ DEFAULT now(),
            error       TEXT
        )
    """)


# ── Problem index helpers ──────────────────────────────────────────────────────

def upsert_problem(
    con: duckdb.DuckDBPyConnection,
    *,
    slug: str,
    section: str,
    rank: int,
    name: str,
    url: str,
    users: int,
    accepted: float,
) -> None:
    con.execute("""
        INSERT INTO problems (slug, section, rank, name, url, users, accepted, fetched_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, now())
        ON CONFLICT (slug, section) DO UPDATE SET
            rank       = excluded.rank,
            name       = excluded.name,
            url        = excluded.url,
            users      = excluded.users,
            accepted   = excluded.accepted,
            fetched_at = excluded.fetched_at
    """, [slug, section, rank, name, url, users, accepted])


def count_problems(con: duckdb.DuckDBPyConnection, section: str | None = None) -> int:
    if section:
        return con.execute(
            "SELECT COUNT(*) FROM problems WHERE section = ?", [section]
        ).fetchone()[0]
    return con.execute("SELECT COUNT(*) FROM problems").fetchone()[0]


def get_unfetched_slugs(
    con_main: duckdb.DuckDBPyConnection,
    con_state: duckdb.DuckDBPyConnection,
    section: str | None = None,
    limit: int | None = None,
    from_rank: int | None = None,
    to_rank: int | None = None,
    include_errors: bool = False,
) -> list[tuple[str, str]]:
    """Return (slug, url) pairs that haven't been successfully fetched yet.

    Filters out any problem whose body is already stored, and any task marked
    ``done`` in the state DB. Optional ``from_rank``/``to_rank`` bound the rank
    range (inclusive). When ``include_errors`` is True, problems marked as
    ``error`` in the state DB are still returned (retry path).
    """
    clauses: list[str] = []
    params: list = []
    if section:
        clauses.append("p.section = ?")
        params.append(section)
    if from_rank is not None:
        clauses.append("p.rank >= ?")
        params.append(from_rank)
    if to_rank is not None:
        clauses.append("p.rank <= ?")
        params.append(to_rank)
    where = ("WHERE " + " AND ".join(clauses)) if clauses else ""

    rows = con_main.execute(f"""
        SELECT DISTINCT p.slug, p.url
        FROM problems p
        LEFT JOIN problem_bodies b ON b.slug = p.slug
        {where}
        ORDER BY p.section, p.rank
    """, params).fetchall()

    skip = set(
        r[0] for r in con_state.execute(
            "SELECT task FROM fetch_state WHERE status = 'done' AND task LIKE 'body:%'"
        ).fetchall()
    )
    if not include_errors:
        skip.update(
            r[0] for r in con_state.execute(
                "SELECT task FROM fetch_state WHERE status = 'error' AND task LIKE 'body:%'"
            ).fetchall()
        )

    result = [
        (slug, url) for slug, url in rows
        if f"body:{slug}" not in skip
    ]

    return result[:limit] if limit else result


# ── Problem body helpers ───────────────────────────────────────────────────────

def upsert_body(
    con: duckdb.DuckDBPyConnection,
    *,
    slug: str,
    url: str,
    html: str,
    markdown: str,
) -> None:
    con.execute("""
        INSERT INTO problem_bodies (slug, url, html, markdown, fetched_at)
        VALUES (?, ?, ?, ?, now())
        ON CONFLICT (slug) DO UPDATE SET
            url        = excluded.url,
            html       = excluded.html,
            markdown   = excluded.markdown,
            fetched_at = excluded.fetched_at
    """, [slug, url, html, markdown])


def get_problems_for_export(
    con: duckdb.DuckDBPyConnection,
    section: str | None = None,
) -> list[dict]:
    where = "WHERE p.section = ?" if section else ""
    params = [section] if section else []
    rows = con.execute(f"""
        SELECT p.slug, p.section, p.rank, p.name, p.url, p.users, p.accepted,
               b.markdown
        FROM problems p
        JOIN problem_bodies b ON b.slug = p.slug
        {where}
        ORDER BY p.section, p.rank
    """, params).fetchall()
    keys = ["slug", "section", "rank", "name", "url", "users", "accepted", "markdown"]
    return [dict(zip(keys, r)) for r in rows]


# ── State helpers ──────────────────────────────────────────────────────────────

def mark_state(
    con: duckdb.DuckDBPyConnection,
    task: str,
    status: str,
    error: str | None = None,
) -> None:
    con.execute("""
        INSERT INTO fetch_state (task, status, attempts, updated_at, error)
        VALUES (?, ?, 1, now(), ?)
        ON CONFLICT (task) DO UPDATE SET
            status     = excluded.status,
            attempts   = fetch_state.attempts + 1,
            updated_at = excluded.updated_at,
            error      = excluded.error
    """, [task, status, error])


def get_state_summary(con: duckdb.DuckDBPyConnection) -> dict:
    rows = con.execute("""
        SELECT status, COUNT(*) FROM fetch_state GROUP BY status
    """).fetchall()
    return {row[0]: row[1] for row in rows}
