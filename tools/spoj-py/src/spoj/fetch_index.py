"""Fetch and store SPOJ problem index pages."""

from __future__ import annotations

import asyncio

from rich.console import Console

from spoj.browser import spoj_browser, fetch_index_page
from spoj.db import open_main, open_state, upsert_problem, mark_state, SECTIONS

console = Console()


async def fetch_section_index(
    section: str,
    max_pages: int = 200,
    headless: bool = True,
    delay: float = 1.5,
) -> int:
    """Fetch all index pages for one section. Returns number of problems found."""
    con_main = open_main()
    con_state = open_state()
    total = 0

    console.print(f"[bold cyan]Fetching index:[/] {section} (up to {max_pages} pages)")

    async with spoj_browser(headless=headless) as ctx:
        for page_num in range(max_pages):
            task_key = f"index:{section}:{page_num}"

            row = con_state.execute(
                "SELECT status FROM fetch_state WHERE task = ?", [task_key]
            ).fetchone()
            if row and row[0] == "done":
                # Verify data actually exists; re-fetch if DB is empty (stale state)
                existing = con_main.execute(
                    "SELECT COUNT(*) FROM problems WHERE section = ? AND rank BETWEEN ? AND ?",
                    [section, page_num * 50 + 1, page_num * 50 + 50],
                ).fetchone()[0]
                if existing > 0:
                    total += existing
                    continue
                # Fall through to re-fetch this page

            try:
                problems = await fetch_index_page(ctx, section, page_num)

                if not problems:
                    console.print(f"  page {page_num}: empty — stopping")
                    mark_state(con_state, task_key, "done")
                    break

                for p in problems:
                    upsert_problem(
                        con_main,
                        slug=p["slug"],
                        section=section,
                        rank=p["rank"],
                        name=p["name"],
                        url=p["url"],
                        users=p["users"],
                        accepted=p["accepted"],
                    )

                con_main.execute("CHECKPOINT")
                mark_state(con_state, task_key, "done")
                total += len(problems)

                console.print(
                    f"  page {page_num:>3}: [green]{len(problems)}[/] problems "
                    f"(rank {problems[0]['rank']}–{problems[-1]['rank']})"
                )

                if len(problems) < 50:
                    break

                import random
                await asyncio.sleep(delay * random.uniform(0.8, 1.4))

            except Exception as exc:
                console.print(f"  [red]page {page_num} error:[/] {exc}")
                mark_state(con_state, task_key, "error", str(exc))
                import random
                await asyncio.sleep(delay * random.uniform(1.5, 2.5))

    con_main.close()
    con_state.close()
    return total


async def fetch_all_indexes(
    sections: list[str] | None = None,
    max_pages: int = 200,
    headless: bool = True,
    delay: float = 1.5,
) -> dict[str, int]:
    """Fetch index for all (or selected) sections."""
    targets = sections or SECTIONS
    results = {}
    for sec in targets:
        count = await fetch_section_index(sec, max_pages=max_pages, headless=headless, delay=delay)
        results[sec] = count
        console.print(f"[bold green]✓[/] {sec}: {count} problems indexed")
    return results
