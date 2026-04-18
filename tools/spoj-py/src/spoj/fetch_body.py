"""Fetch and store SPOJ problem bodies in parallel, with streaming export.

Two fetch backends are available:

* **chrome** (default) — :class:`spoj.chrome_http.ChromeClient` uses
  ``curl_cffi`` with Chrome TLS fingerprint + cookies pulled from the user's
  real Chrome. This is what actually bypasses SPOJ's Cloudflare protection
  today. No Playwright/Patchright involved.
* **patchright** — legacy path using the stealth Chromium browser. Kept for
  environments without a real Chrome install, or for index/page operations.

The command flow is:

1. If backend is chrome, build a :class:`ChromeClient` (which loads cookies
   from Chrome). Probe once; if blocked, instruct the user to solve CF in
   Chrome.
2. Run a worker pool fetching bodies through the client. On CF blocks mid-run,
   pause, refresh cookies, and retry.
3. Stream each body as a Markdown file as soon as it's stored.
"""

from __future__ import annotations

import asyncio
import random
from pathlib import Path

from rich.console import Console
from rich.progress import (
    Progress, SpinnerColumn, TextColumn, BarColumn,
    MofNCompleteColumn, TimeRemainingColumn,
)

from spoj.browser import (
    spoj_browser,
    fetch_problem_body,
    fetch_problem_body_http,
    html_to_markdown,
    warmup as browser_warmup,
    has_cf_clearance,
    is_http_cleared,
    CloudflareBlocked,
)
from spoj.chrome_http import ChromeClient, probe as chrome_probe
from spoj.db import (
    open_main, open_state,
    upsert_body, mark_state, get_unfetched_slugs,
)

console = Console()


def _stream_export_one(con_main, slug: str, out_dir: Path | None = None) -> None:
    """Write a problem's markdown file immediately after its body is stored."""
    from spoj.db import EXPORT_DIR
    from spoj.export import _TEMPLATE

    base = out_dir or EXPORT_DIR
    row = con_main.execute("""
        SELECT p.slug, p.section, p.rank, p.name, p.url, p.users, p.accepted, b.markdown
        FROM problems p
        JOIN problem_bodies b ON b.slug = p.slug
        WHERE p.slug = ?
        LIMIT 1
    """, [slug]).fetchone()

    if not row:
        return

    slug_, section, rank, name, url, users, accepted, markdown = row
    sec_dir = base / section
    sec_dir.mkdir(parents=True, exist_ok=True)

    fname = f"{rank:05d}_{slug_}.md"
    fpath = sec_dir / fname
    fpath.write_text(
        _TEMPLATE.format(
            name=name or slug_,
            slug=slug_,
            section=section,
            rank=rank,
            users=users or 0,
            accepted=accepted or 0.0,
            url=url,
            body=markdown or "_No content available._",
        ),
        encoding="utf-8",
    )


# ── Chrome (curl_cffi) backend ────────────────────────────────────────────────

async def fetch_bodies_chrome(
    section: str | None = None,
    limit: int | None = None,
    delay: float = 0.8,
    concurrency: int = 8,
    retry_errors: bool = False,
    stream_export: bool = True,
    out_dir: Path | None = None,
    from_rank: int | None = None,
    to_rank: int | None = None,
    impersonate: str = "chrome146",
    cookies_browser: str = "chrome",
) -> int:
    """Fetch bodies via the Chrome-TLS HTTP backend. Returns count written."""
    con_main = open_main()
    con_state = open_state()

    targets = get_unfetched_slugs(
        con_main, con_state,
        section=section,
        limit=limit,
        from_rank=from_rank,
        to_rank=to_rank,
        include_errors=retry_errors,
    )

    if not targets:
        console.print("[yellow]Nothing to fetch — all bodies already stored.[/]")
        con_main.close()
        con_state.close()
        return 0

    try:
        client = ChromeClient(
            impersonate=impersonate,
            cookies_browser=cookies_browser,
        )
    except RuntimeError as exc:
        console.print(f"[bold red]Chrome backend unavailable:[/] {exc}")
        con_main.close()
        con_state.close()
        return 0

    console.print(
        f"[bold cyan]Fetching bodies (chrome/{impersonate}):[/] {len(targets)} problems "
        f"[dim](concurrency={concurrency}, delay={delay}s)[/]"
    )

    # Startup probe with backoff so a temporary CF rate-limit doesn't abort.
    probe_ok = False
    for attempt, wait in enumerate((0, 30, 60, 120, 300), start=1):
        if wait:
            console.print(
                f"[yellow]Startup probe blocked — sleeping {wait}s (attempt {attempt}/5)…[/]"
            )
            await asyncio.sleep(wait)
        try:
            client.refresh_cookies()
        except Exception:
            pass
        if await chrome_probe(client):
            probe_ok = True
            break
    if not probe_ok:
        console.print(
            "[bold red]CF probe still failing after 5 tries.[/] "
            f"Open {BASE_URL_HOMEPAGE} in Chrome, solve the Cloudflare challenge, "
            "then retry."
        )
        await client.close()
        con_main.close()
        con_state.close()
        return 0

    work_q: asyncio.Queue[tuple[str, str] | None] = asyncio.Queue()
    for item in targets:
        await work_q.put(item)

    result_q: asyncio.Queue = asyncio.Queue(maxsize=concurrency * 4)
    fetched_count = 0
    abort_flag = asyncio.Event()
    cf_event = asyncio.Event()
    cf_event.set()
    cf_lock = asyncio.Lock()

    async def handle_cf(slug: str) -> bool:
        """Coordinate CF recovery across workers.

        On first CF hit: pauses all workers, refreshes cookies, and probes.
        If still blocked, backs off exponentially (30s → 60s → 120s → 300s)
        before re-probing. Aborts only after 4 consecutive failed probes.
        """
        async with cf_lock:
            if cf_event.is_set():
                cf_event.clear()
                try:
                    for attempt, wait in enumerate((0, 30, 60, 120, 300), start=1):
                        if wait:
                            console.print(
                                f"[yellow]CF still blocking — sleeping {wait}s (attempt {attempt}/5)…[/]"
                            )
                            await asyncio.sleep(wait)
                        console.print(
                            f"[yellow]CF detected on {slug} — refreshing cookies from Chrome…[/]"
                        )
                        try:
                            client.refresh_cookies()
                        except Exception as exc:
                            console.print(f"  [red]cookie refresh failed:[/] {exc}")
                            continue
                        if await chrome_probe(client):
                            console.print(f"[green]✓ CF cleared (attempt {attempt}).[/]")
                            return True
                    console.print(
                        "[bold red]× Still blocked after 5 attempts.[/] "
                        "In Chrome, visit https://www.spoj.com/problems/TEST/ and "
                        "solve the Cloudflare challenge, then re-run."
                    )
                    abort_flag.set()
                    return False
                finally:
                    cf_event.set()
        await cf_event.wait()
        return not abort_flag.is_set()

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeRemainingColumn(),
        console=console,
        refresh_per_second=4,
    ) as progress:
        job = progress.add_task("Fetching...", total=len(targets))

        async def worker(wid: int) -> None:
            while not abort_flag.is_set():
                await cf_event.wait()
                try:
                    item = work_q.get_nowait()
                except asyncio.QueueEmpty:
                    break
                slug, url = item
                task_key = f"body:{slug}"
                try:
                    try:
                        body, final_url = await client.fetch_body(slug, url)
                    except CloudflareBlocked:
                        cleared = await handle_cf(slug)
                        if not cleared:
                            await result_q.put((slug, url, None, "cf-blocked", task_key))
                            continue
                        body, final_url = await client.fetch_body(slug, url)
                    await result_q.put((slug, final_url, body, None, task_key))
                except Exception as exc:
                    await result_q.put((slug, url, None, str(exc), task_key))
                finally:
                    work_q.task_done()
                    await asyncio.sleep(delay * random.uniform(0.7, 1.3))

        async def writer() -> None:
            nonlocal fetched_count
            expected = len(targets)
            done_count = 0
            while done_count < expected:
                item = await result_q.get()
                slug, url, html, error, task_key = item

                if error:
                    console.print(f"  [red]{slug}:[/] {error}")
                    mark_state(con_state, task_key, "error", error)
                elif html and html.strip():
                    markdown = html_to_markdown(html)
                    upsert_body(con_main, slug=slug, url=url, html=html, markdown=markdown)
                    con_main.execute("CHECKPOINT")
                    mark_state(con_state, task_key, "done")
                    fetched_count += 1
                    if stream_export:
                        try:
                            _stream_export_one(con_main, slug, out_dir)
                        except Exception as exp:
                            console.print(f"  [yellow]export {slug}:[/] {exp}")
                else:
                    mark_state(con_state, task_key, "error", "empty body")

                progress.update(job, advance=1, description=f"[cyan]{slug}[/]")
                result_q.task_done()
                done_count += 1

        workers = [asyncio.create_task(worker(i)) for i in range(concurrency)]
        writer_task = asyncio.create_task(writer())

        await asyncio.gather(*workers)
        try:
            await asyncio.wait_for(writer_task, timeout=10)
        except asyncio.TimeoutError:
            writer_task.cancel()
            try:
                await writer_task
            except (asyncio.CancelledError, Exception):
                pass

    await client.close()
    con_main.close()
    con_state.close()
    console.print(f"[bold green]✓[/] {fetched_count}/{len(targets)} bodies fetched and exported")
    return fetched_count


BASE_URL_HOMEPAGE = "https://www.spoj.com/problems/classical/"


# ── Patchright backend (legacy / fallback) ────────────────────────────────────

async def _recover_from_cf(ctx, headless: bool) -> bool:
    try:
        ok = await browser_warmup(ctx, timeout=300_000 if not headless else 30_000)
    except Exception as exc:
        console.print(f"  [red]CF recovery failed:[/] {exc}")
        return False
    return ok


async def fetch_bodies_patchright(
    section: str | None = None,
    limit: int | None = None,
    headless: bool = True,
    delay: float = 1.5,
    concurrency: int = 3,
    retry_errors: bool = False,
    stream_export: bool = True,
    out_dir: Path | None = None,
    from_rank: int | None = None,
    to_rank: int | None = None,
    pre_warmup: bool = True,
    use_http: bool = True,
) -> int:
    """Legacy Patchright-based fetcher. See :func:`fetch_bodies_chrome` for the
    recommended path — Patchright's Chromium is now often CF-blocked."""
    con_main = open_main()
    con_state = open_state()

    targets = get_unfetched_slugs(
        con_main, con_state,
        section=section,
        limit=limit,
        from_rank=from_rank,
        to_rank=to_rank,
        include_errors=retry_errors,
    )

    if not targets:
        console.print("[yellow]Nothing to fetch — all bodies already stored.[/]")
        con_main.close()
        con_state.close()
        return 0

    mode = "HTTP" if use_http else "page"
    console.print(
        f"[bold cyan]Fetching bodies (patchright/{mode}):[/] {len(targets)} problems "
        f"[dim](concurrency={concurrency}, delay={delay}s)[/]"
    )

    work_q: asyncio.Queue[tuple[str, str] | None] = asyncio.Queue()
    for item in targets:
        await work_q.put(item)

    result_q: asyncio.Queue = asyncio.Queue(maxsize=concurrency * 4)
    fetched_count = 0
    cf_event = asyncio.Event()
    cf_event.set()
    cf_lock = asyncio.Lock()
    abort_flag = asyncio.Event()

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeRemainingColumn(),
        console=console,
        refresh_per_second=4,
    ) as progress:
        job = progress.add_task("Fetching...", total=len(targets))

        async with spoj_browser(headless=headless) as ctx:

            if pre_warmup:
                if use_http and await is_http_cleared(ctx):
                    console.print("[green]✓ HTTP path already cleared — skipping warmup.[/]")
                else:
                    console.print("[cyan]Warming up — visiting SPOJ to resolve CF…[/]")
                    ok = await browser_warmup(ctx, timeout=300_000 if not headless else 30_000)
                    if not ok and headless:
                        console.print(
                            "[bold red]CF challenge not cleared in headless mode.[/] "
                            "Try `--backend chrome` or run `spoj warmup` headed."
                        )
                        con_main.close()
                        con_state.close()
                        return 0
                    if use_http and not await is_http_cleared(ctx):
                        console.print(
                            "[yellow]HTTP probe still blocked — falling back to --page mode.[/]"
                        )
                        use_http = False

            async def handle_cf(slug: str) -> bool:
                async with cf_lock:
                    if cf_event.is_set():
                        cf_event.clear()
                        try:
                            console.print(
                                f"[yellow]CF triggered on {slug} — pausing workers for recovery…[/]"
                            )
                            ok = await _recover_from_cf(ctx, headless)
                            if not ok:
                                abort_flag.set()
                            return ok
                        finally:
                            cf_event.set()
                await cf_event.wait()
                return not abort_flag.is_set()

            async def worker(wid: int) -> None:
                while not abort_flag.is_set():
                    await cf_event.wait()
                    try:
                        item = work_q.get_nowait()
                    except asyncio.QueueEmpty:
                        break
                    slug, url = item
                    task_key = f"body:{slug}"
                    try:
                        if use_http:
                            try:
                                html, final_url = await fetch_problem_body_http(ctx, slug, url)
                            except CloudflareBlocked:
                                cleared = await handle_cf(slug)
                                if not cleared:
                                    await result_q.put((slug, url, None, "cf-blocked", task_key))
                                    continue
                                html, final_url = await fetch_problem_body(ctx, slug, url)
                        else:
                            html, final_url = await fetch_problem_body(ctx, slug, url)
                        await result_q.put((slug, final_url, html, None, task_key))
                    except Exception as exc:
                        await result_q.put((slug, url, None, str(exc), task_key))
                    finally:
                        work_q.task_done()
                        await asyncio.sleep(delay * random.uniform(0.8, 1.4))

            async def writer() -> None:
                nonlocal fetched_count
                expected = len(targets)
                done_count = 0
                while done_count < expected:
                    item = await result_q.get()
                    slug, url, html, error, task_key = item

                    if error:
                        console.print(f"  [red]{slug}:[/] {error}")
                        mark_state(con_state, task_key, "error", error)
                    elif html and html.strip():
                        markdown = html_to_markdown(html)
                        upsert_body(con_main, slug=slug, url=url, html=html, markdown=markdown)
                        con_main.execute("CHECKPOINT")
                        mark_state(con_state, task_key, "done")
                        fetched_count += 1
                        if stream_export:
                            try:
                                _stream_export_one(con_main, slug, out_dir)
                            except Exception as exp:
                                console.print(f"  [yellow]export {slug}:[/] {exp}")
                    else:
                        mark_state(con_state, task_key, "error", "empty body")

                    progress.update(job, advance=1, description=f"[cyan]{slug}[/]")
                    result_q.task_done()
                    done_count += 1

            workers = [asyncio.create_task(worker(i)) for i in range(concurrency)]
            writer_task = asyncio.create_task(writer())
            await asyncio.gather(*workers)
            try:
                await asyncio.wait_for(writer_task, timeout=10)
            except asyncio.TimeoutError:
                writer_task.cancel()
                try:
                    await writer_task
                except (asyncio.CancelledError, Exception):
                    pass

    con_main.close()
    con_state.close()
    console.print(f"[bold green]✓[/] {fetched_count}/{len(targets)} bodies fetched and exported")
    return fetched_count


# ── Dispatcher + single-slug ──────────────────────────────────────────────────

async def fetch_bodies(
    *,
    backend: str = "chrome",
    impersonate: str = "chrome146",
    cookies_browser: str = "chrome",
    **kwargs,
) -> int:
    """Fetch bodies using the selected backend."""
    if backend == "chrome":
        chrome_kwargs = {
            k: v for k, v in kwargs.items()
            if k in {
                "section", "limit", "delay", "concurrency", "retry_errors",
                "stream_export", "out_dir", "from_rank", "to_rank",
            }
        }
        return await fetch_bodies_chrome(
            impersonate=impersonate,
            cookies_browser=cookies_browser,
            **chrome_kwargs,
        )
    return await fetch_bodies_patchright(**kwargs)


async def fetch_single(
    slug: str,
    *,
    backend: str = "chrome",
    impersonate: str = "chrome146",
    cookies_browser: str = "chrome",
    headless: bool = True,
    use_http: bool = True,
    stream_export: bool = True,
) -> bool:
    """Fetch a single problem body by slug."""
    con_main = open_main()
    con_state = open_state()

    row = con_main.execute(
        "SELECT url FROM problems WHERE slug = ? LIMIT 1", [slug]
    ).fetchone()
    url = row[0] if row else f"https://www.spoj.com/problems/{slug}/"

    try:
        if backend == "chrome":
            try:
                async with ChromeClient(
                    impersonate=impersonate,
                    cookies_browser=cookies_browser,
                ) as client:
                    body, final_url = await client.fetch_body(slug, url)
            except RuntimeError as exc:
                console.print(f"[red]{slug}:[/] {exc}")
                mark_state(con_state, f"body:{slug}", "error", str(exc))
                return False
        else:
            async with spoj_browser(headless=headless) as ctx:
                try:
                    if use_http:
                        try:
                            body, final_url = await fetch_problem_body_http(ctx, slug, url)
                        except CloudflareBlocked:
                            console.print("[yellow]CF blocked; falling back to page fetch…[/]")
                            body, final_url = await fetch_problem_body(ctx, slug, url)
                    else:
                        body, final_url = await fetch_problem_body(ctx, slug, url)
                except Exception as exc:
                    console.print(f"[red]Error fetching {slug}:[/] {exc}")
                    mark_state(con_state, f"body:{slug}", "error", str(exc))
                    return False

        if not body.strip():
            console.print(f"[red]Empty body for {slug}[/]")
            mark_state(con_state, f"body:{slug}", "error", "empty body")
            return False

        markdown = html_to_markdown(body)
        upsert_body(con_main, slug=slug, url=final_url, html=body, markdown=markdown)
        con_main.execute("CHECKPOINT")
        mark_state(con_state, f"body:{slug}", "done")
        if stream_export:
            _stream_export_one(con_main, slug)
        console.print(f"[green]✓[/] {slug} — {len(markdown)} chars")
        return True
    except Exception as exc:
        console.print(f"[red]Error fetching {slug}:[/] {exc}")
        mark_state(con_state, f"body:{slug}", "error", str(exc))
        return False
    finally:
        con_main.close()
        con_state.close()


async def run_warmup(headless: bool = False, url: str | None = None) -> bool:
    """Legacy CLI entry for the Patchright-based ``spoj warmup`` command."""
    async with spoj_browser(headless=headless) as ctx:
        ok = await browser_warmup(ctx, url=url, timeout=600_000)
        if not ok:
            console.print("[bold red]× CF not cleared.[/] Try again, or retry headed.")
            return False
        console.print("[bold green]✓ CF cleared (page).[/] Probing HTTP path…")
        http_ok = await is_http_cleared(ctx)
        if http_ok:
            console.print("[bold green]✓ HTTP path works.[/]")
        else:
            console.print(
                "[yellow]× HTTP path still blocked[/] — try `--backend chrome` with real Chrome cookies."
            )
        return True
