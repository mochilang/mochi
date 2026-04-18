"""Export SPOJ problems from DuckDB to Markdown files."""

from __future__ import annotations

from pathlib import Path

from rich.console import Console

from spoj.db import open_main, get_problems_for_export, EXPORT_DIR

console = Console()

_TEMPLATE = """\
# {name}

**Slug:** `{slug}` | **Section:** {section} | **Rank:** {rank}
**Users solved:** {users:,} | **Acceptance:** {accepted:.1f}%
**URL:** {url}

---

{body}
"""


def export_all(
    section: str | None = None,
    force: bool = False,
    out_dir: Path | None = None,
) -> int:
    """Export all problems with bodies to Markdown files.

    Directory layout:
        {out_dir}/{section}/{rank:05d}_{slug}.md

    Returns count of files written.
    """
    base = Path(out_dir) if out_dir else EXPORT_DIR
    con = open_main()
    problems = get_problems_for_export(con, section=section)
    con.close()

    if not problems:
        console.print("[yellow]No problems with bodies to export.[/]")
        return 0

    written = 0
    skipped = 0

    for p in problems:
        sec_dir = base / p["section"]
        sec_dir.mkdir(parents=True, exist_ok=True)

        fname = f"{p['rank']:05d}_{p['slug']}.md"
        fpath = sec_dir / fname

        if fpath.exists() and not force:
            skipped += 1
            continue

        body = p["markdown"] or "_No content available._"

        content = _TEMPLATE.format(
            name=p["name"],
            slug=p["slug"],
            section=p["section"],
            rank=p["rank"],
            users=p["users"] or 0,
            accepted=p["accepted"] or 0.0,
            url=p["url"],
            body=body,
        )

        fpath.write_text(content, encoding="utf-8")
        written += 1

    console.print(
        f"[bold green]✓[/] Exported {written} files to [cyan]{base}[/]"
        + (f" ({skipped} skipped, use --force to overwrite)" if skipped else "")
    )
    return written


def export_index_md(section: str | None = None, out_dir: Path | None = None) -> None:
    """Write a combined index.md per section (rank-ordered table)."""
    base = Path(out_dir) if out_dir else EXPORT_DIR
    con = open_main()

    sections_to_export: list[str]
    if section:
        sections_to_export = [section]
    else:
        rows = con.execute(
            "SELECT DISTINCT section FROM problems ORDER BY section"
        ).fetchall()
        sections_to_export = [r[0] for r in rows]

    for sec in sections_to_export:
        rows = con.execute("""
            SELECT p.rank, p.slug, p.name, p.url, p.users, p.accepted,
                   (b.slug IS NOT NULL) AS has_body
            FROM problems p
            LEFT JOIN problem_bodies b ON b.slug = p.slug
            WHERE p.section = ?
            ORDER BY p.rank
        """, [sec]).fetchall()

        if not rows:
            continue

        sec_dir = base / sec
        sec_dir.mkdir(parents=True, exist_ok=True)
        idx_path = sec_dir / "index.md"

        lines = [
            f"# SPOJ {sec.capitalize()} — Problem Index\n",
            f"Total: {len(rows)} problems\n",
            "| Rank | Slug | Name | Users | ACC% | Body |\n",
            "|------|------|------|------:|-----:|------|\n",
        ]
        for rank, slug, name, url, users, accepted, has_body in rows:
            body_mark = "✓" if has_body else ""
            lines.append(
                f"| {rank} | [{slug}]({url}) | {name} "
                f"| {(users or 0):,} | {(accepted or 0):.1f} | {body_mark} |\n"
            )

        idx_path.write_text("".join(lines), encoding="utf-8")
        console.print(f"  [cyan]{idx_path}[/] — {len(rows)} rows")

    con.close()
