"""Click CLI for the SPOJ fetcher."""

from __future__ import annotations

import asyncio
from pathlib import Path

import click
from rich.console import Console
from rich.table import Table

from spoj.db import open_main, open_state, count_problems, get_state_summary, SECTIONS, MAIN_DB, STATE_DB, EXPORT_DIR

console = Console()


@click.group()
def main() -> None:
    """SPOJ problem fetcher — index, download, and export problems."""


# ── spoj index ─────────────────────────────────────────────────────────────────

@main.command()
@click.option(
    "--section", "-s",
    default="all",
    type=click.Choice(["all"] + SECTIONS),
    show_default=True,
    help="Section to index (or 'all').",
)
@click.option("--max-pages", default=200, show_default=True, help="Max pages per section (50 problems each).")
@click.option("--headless/--headed", default=True, show_default=True, help="Run browser headless or visible.")
@click.option("--delay", default=1.5, show_default=True, type=float, help="Seconds between page requests.")
def index(section: str, max_pages: int, headless: bool, delay: float) -> None:
    """Fetch problem index pages from SPOJ and store in DuckDB."""
    from spoj.fetch_index import fetch_section_index, fetch_all_indexes

    if section == "all":
        results = asyncio.run(
            fetch_all_indexes(max_pages=max_pages, headless=headless, delay=delay)
        )
        table = Table(title="Index Summary", show_header=True)
        table.add_column("Section")
        table.add_column("Problems", justify="right")
        for sec, count in results.items():
            table.add_row(sec, str(count))
        console.print(table)
    else:
        count = asyncio.run(
            fetch_section_index(section, max_pages=max_pages, headless=headless, delay=delay)
        )
        console.print(f"[bold green]Done:[/] {count} problems indexed for {section}")


# ── spoj fetch ─────────────────────────────────────────────────────────────────

@main.command()
@click.option("--slug", "-p", default=None, help="Fetch a single problem by slug (e.g. FCTRL).")
@click.option(
    "--section", "-s",
    default=None,
    type=click.Choice(SECTIONS),
    help="Only fetch bodies for problems in this section.",
)
@click.option("--limit", "-n", default=None, type=int, help="Max number of problems to fetch.")
@click.option("--from-rank", default=None, type=int, help="Only fetch ranks >= this value.")
@click.option("--to-rank", default=None, type=int, help="Only fetch ranks <= this value.")
@click.option(
    "--backend",
    type=click.Choice(["chrome", "patchright"]),
    default="chrome",
    show_default=True,
    help="chrome = curl_cffi + cookies from your real Chrome (recommended, "
         "bypasses CF). patchright = stealth headless Chromium (legacy).",
)
@click.option("--impersonate", default="chrome146", show_default=True,
              help="curl_cffi Chrome profile (chrome146, chrome145, chrome142, …).")
@click.option("--cookies-browser", default="chrome", show_default=True,
              type=click.Choice(["chrome", "firefox", "brave", "edge", "safari", "chromium", "opera"]),
              help="Browser to read CF cookies from.")
@click.option("--headless/--headed", default=True, show_default=True,
              help="(patchright backend only)")
@click.option("--delay", default=0.8, show_default=True, type=float, help="Seconds between fetches.")
@click.option("--retry-errors", is_flag=True, default=False, help="Retry previously-errored problems.")
@click.option("--concurrency", "-c", default=8, show_default=True, type=int, help="Parallel requests.")
@click.option("--http/--page", "use_http", default=True, show_default=True,
              help="(patchright backend only) HTTP vs. per-page navigation.")
@click.option("--pre-warmup/--no-pre-warmup", default=True, show_default=True,
              help="(patchright backend only) Visit SPOJ homepage first to resolve CF.")
def fetch(
    slug: str | None,
    section: str | None,
    limit: int | None,
    from_rank: int | None,
    to_rank: int | None,
    backend: str,
    impersonate: str,
    cookies_browser: str,
    headless: bool,
    delay: float,
    retry_errors: bool,
    concurrency: int,
    use_http: bool,
    pre_warmup: bool,
) -> None:
    """Fetch problem statement bodies and store HTML + Markdown in DuckDB."""
    from spoj.fetch_body import fetch_bodies, fetch_single

    if slug:
        ok = asyncio.run(fetch_single(
            slug.upper(),
            backend=backend,
            impersonate=impersonate,
            cookies_browser=cookies_browser,
            headless=headless,
            use_http=use_http,
        ))
        raise SystemExit(0 if ok else 1)

    asyncio.run(
        fetch_bodies(
            backend=backend,
            impersonate=impersonate,
            cookies_browser=cookies_browser,
            section=section,
            limit=limit,
            headless=headless,
            delay=delay,
            concurrency=concurrency,
            retry_errors=retry_errors,
            from_rank=from_rank,
            to_rank=to_rank,
            pre_warmup=pre_warmup,
            use_http=use_http,
        )
    )


# ── spoj warmup ────────────────────────────────────────────────────────────────

@main.command()
@click.option("--headless/--headed", default=False, show_default=True,
              help="Run browser visible (recommended) so you can solve CF manually.")
@click.option("--url", default=None, help="URL to warm up on (defaults to SPOJ classical list).")
def warmup(headless: bool, url: str | None) -> None:
    """Open SPOJ in a real browser and wait for you to solve any CF challenge.

    The resulting ``cf_clearance`` cookie is stored in the persistent browser
    profile so later `spoj fetch` runs — even headless — can reuse it.
    """
    from spoj.fetch_body import run_warmup
    ok = asyncio.run(run_warmup(headless=headless, url=url))
    raise SystemExit(0 if ok else 1)


# ── spoj export ────────────────────────────────────────────────────────────────

@main.command()
@click.option(
    "--section", "-s",
    default=None,
    type=click.Choice(SECTIONS),
    help="Only export this section.",
)
@click.option("--out-dir", default=None, type=click.Path(), help=f"Output dir (default: {EXPORT_DIR}).")
@click.option("--force", is_flag=True, default=False, help="Overwrite existing files.")
@click.option("--index-only", is_flag=True, default=False, help="Only write index.md files, not problem bodies.")
def export(section: str | None, out_dir: str | None, force: bool, index_only: bool) -> None:
    """Export problems from DuckDB to Markdown files.

    Layout: {out-dir}/{section}/{rank:05d}_{slug}.md
    """
    from spoj.export import export_all, export_index_md

    path = Path(out_dir) if out_dir else None

    export_index_md(section=section, out_dir=path)

    if not index_only:
        export_all(section=section, force=force, out_dir=path)


# ── spoj status ────────────────────────────────────────────────────────────────

@main.command()
def status() -> None:
    """Show fetch progress and database statistics."""
    con_main = open_main()
    con_state = open_state()

    console.print(f"\n[bold]Database paths:[/]")
    console.print(f"  Main:   [cyan]{MAIN_DB}[/]")
    console.print(f"  State:  [cyan]{STATE_DB}[/]")
    console.print(f"  Export: [cyan]{EXPORT_DIR}[/]\n")

    # Problems table
    table = Table(title="Problems Indexed", show_header=True)
    table.add_column("Section")
    table.add_column("Problems", justify="right")
    table.add_column("With Body", justify="right")
    table.add_column("Missing", justify="right")

    total_p = 0
    total_b = 0

    for sec in SECTIONS:
        n_probs = count_problems(con_main, sec)
        n_bodies = con_main.execute("""
            SELECT COUNT(DISTINCT p.slug)
            FROM problems p
            JOIN problem_bodies b ON b.slug = p.slug
            WHERE p.section = ?
        """, [sec]).fetchone()[0]
        missing = n_probs - n_bodies
        table.add_row(sec, str(n_probs), str(n_bodies), str(missing) if missing > 0 else "[green]0[/]")
        total_p += n_probs
        total_b += n_bodies

    table.add_section()
    table.add_row("[bold]Total[/]", str(total_p), str(total_b), str(total_p - total_b))
    console.print(table)

    # State summary
    state_summary = get_state_summary(con_state)
    if state_summary:
        console.print("\n[bold]Fetch State:[/]")
        for status_val, count in sorted(state_summary.items()):
            color = {"done": "green", "error": "red", "pending": "yellow"}.get(status_val, "white")
            console.print(f"  [{color}]{status_val}[/]: {count}")

    # Recent errors
    errors = con_state.execute("""
        SELECT task, error, updated_at
        FROM fetch_state
        WHERE status = 'error'
        ORDER BY updated_at DESC
        LIMIT 10
    """).fetchall()

    if errors:
        console.print("\n[bold red]Recent Errors:[/]")
        for task, error, updated_at in errors:
            console.print(f"  [red]{task}[/]: {error}")

    con_main.close()
    con_state.close()


# ── spoj query ─────────────────────────────────────────────────────────────────

@main.command()
@click.argument("sql")
def query(sql: str) -> None:
    """Run a raw SQL query against the main DuckDB database."""
    con = open_main()
    try:
        result = con.execute(sql).fetchall()
        desc = con.description
        if desc:
            headers = [d[0] for d in desc]
            table = Table(*headers, show_header=True)
            for row in result:
                table.add_row(*[str(v) for v in row])
            console.print(table)
        else:
            console.print(f"[green]OK[/] ({len(result)} rows)")
    except Exception as e:
        console.print(f"[red]Error:[/] {e}")
    finally:
        con.close()


# ── spoj reset-errors ──────────────────────────────────────────────────────────

@main.command("reset-errors")
@click.option("--task-prefix", default="body:", help="Reset tasks matching this prefix.")
def reset_errors(task_prefix: str) -> None:
    """Mark all errored tasks as pending so they will be retried."""
    con_state = open_state()
    n = con_state.execute(
        "UPDATE fetch_state SET status = 'pending', error = NULL WHERE status = 'error' AND task LIKE ?",
        [f"{task_prefix}%"],
    ).rowcount
    con_state.execute("CHECKPOINT")
    con_state.close()
    console.print(f"[green]Reset {n} errored tasks to pending.[/]")
