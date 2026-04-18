"""Patchright browser context manager for SPOJ fetching.

Two fetch paths are exposed:

* ``fetch_problem_body_http`` — fast path using ``ctx.request.get`` (no page
  render). Reuses the browser context's cookies so it benefits from a prior
  Cloudflare clearance. Use this once the profile has a valid ``cf_clearance``
  cookie (call :func:`warmup` if unsure).
* ``fetch_problem_body`` — fallback page-based path that can solve CF challenges
  inline when running headed.
"""

from __future__ import annotations

import asyncio
import re
from contextlib import asynccontextmanager
from pathlib import Path
from typing import AsyncIterator

from patchright.async_api import async_playwright, BrowserContext, Page

BASE_URL = "https://www.spoj.com"

# Persistent profile so CF cookies survive across runs
_DEFAULT_PROFILE = Path.home() / "data" / "spoj" / "browser-profile"

# Set to True when running headless — controls CF wait behavior
_running_headless: bool = True

# Markers that indicate a Cloudflare challenge/interstitial body rather
# than the actual page content.
_CF_MARKERS = (
    "Just a moment",
    "Attention Required",
    "cf-browser-verification",
    "challenge-platform",
    "__cf_chl_opt",
    "cdn-cgi/challenge-platform",
    "Enable JavaScript and cookies to continue",
)


@asynccontextmanager
async def spoj_browser(
    headless: bool = True,
    slow_mo: int = 0,
    profile_dir: Path | None = None,
) -> AsyncIterator[BrowserContext]:
    """Launch a stealth Patchright browser with a persistent profile.

    A persistent context (launch_persistent_context) stores cookies/localStorage
    across runs so a Cloudflare clearance solved once is reused automatically.

    On first run against a CF-protected page, pass headless=False so the browser
    window appears and you can complete the challenge manually.
    """
    global _running_headless
    _running_headless = headless
    profile = profile_dir or _DEFAULT_PROFILE
    profile.mkdir(parents=True, exist_ok=True)

    async with async_playwright() as p:
        ctx = await p.chromium.launch_persistent_context(
            user_data_dir=str(profile),
            headless=headless,
            slow_mo=slow_mo,
            channel="chrome",          # use system Chrome for better fingerprinting
            viewport={"width": 1280, "height": 900},
            locale="en-US",
            timezone_id="America/New_York",
            args=[
                "--disable-blink-features=AutomationControlled",
                "--no-sandbox",
                "--disable-dev-shm-usage",
                *(["--window-position=9999,9999"] if headless else ["--window-position=100,100", "--window-size=1280,900"]),
                "--no-first-run",
                "--no-default-browser-check",
            ],
            ignore_default_args=["--enable-automation"],
            extra_http_headers={
                "Accept-Language": "en-US,en;q=0.9",
                "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            },
        )
        try:
            await _inject_cookies_from_file(ctx)
            yield ctx
        finally:
            await ctx.close()


async def _inject_cookies_from_file(ctx: BrowserContext) -> None:
    """Inject SPOJ cookies from ~/data/spoj/cookies.json into the browser context."""
    import json
    cookies_file = Path.home() / "data" / "spoj" / "cookies.json"
    if not cookies_file.exists():
        return
    try:
        raw = json.loads(cookies_file.read_text())
        playwright_cookies = [
            {
                "name": c["name"],
                "value": c["value"],
                "domain": c.get("domain", ".spoj.com"),
                "path": c.get("path", "/"),
                "httpOnly": c.get("httpOnly", False),
                "secure": c.get("secure", True),
                "sameSite": "None",
            }
            for c in raw
        ]
        await ctx.add_cookies(playwright_cookies)
        print(f"[cookies] Injected {len(playwright_cookies)} cookies from cookies.json", flush=True)
    except Exception as e:
        print(f"[cookies] Failed to inject cookies: {e}", flush=True)


def looks_like_cf(html: str | None) -> bool:
    """Return True if HTML looks like a Cloudflare challenge page."""
    if not html:
        return False
    head = html[:4000]
    return any(marker in head for marker in _CF_MARKERS)


def extract_problem_body(html: str) -> str:
    """Extract the ``#problem-body`` inner HTML using BeautifulSoup.

    Returns an empty string if the element is missing.
    """
    from bs4 import BeautifulSoup

    soup = BeautifulSoup(html, "html.parser")
    el = soup.find(id="problem-body")
    if not el:
        return ""
    return el.decode_contents()


async def fetch_page(ctx: BrowserContext, url: str) -> Page:
    """Open URL in a new page, wait through any Cloudflare challenge."""
    import random
    page = await ctx.new_page()
    await page.goto(url, wait_until="domcontentloaded", timeout=60_000)
    await _wait_through_cloudflare(page, timeout=120_000)
    await asyncio.sleep(random.uniform(0.3, 0.9))
    return page


async def _wait_through_cloudflare(page: Page, timeout: int = 45_000) -> None:
    """Block until Cloudflare challenge resolves (or timeout).

    In headed mode (window-position=100,100), waits indefinitely so the user
    can manually solve the challenge. In headless mode, times out after `timeout`ms.
    """
    import time
    try:
        title = await page.title()
        if "Just a moment" in title or "Attention Required" in title:
            if not _running_headless:
                print(f"\n[CF] *** CLOUDFLARE CHALLENGE DETECTED ***", flush=True)
                print(f"[CF] URL: {page.url}", flush=True)
                print(f"[CF] Please solve the challenge in the browser window, then wait...", flush=True)
                while True:
                    try:
                        await asyncio.sleep(2)
                        cur_title = await page.title()
                        if "Just a moment" not in cur_title and "Attention Required" not in cur_title:
                            print(f"[CF] ✓ Challenge resolved! New title: {cur_title!r}", flush=True)
                            await asyncio.sleep(1)
                            break
                    except Exception:
                        break
            else:
                print(f"[CF] Challenge detected on {page.url} — waiting up to {timeout//1000}s...", flush=True)
                t0 = time.monotonic()
                await asyncio.sleep(2)
                try:
                    await page.wait_for_function(
                        "() => !document.title.includes('Just a moment') && "
                        "       !document.title.includes('Attention Required')",
                        timeout=timeout,
                    )
                    elapsed = time.monotonic() - t0
                    new_title = await page.title()
                    print(f"[CF] Challenge resolved in {elapsed:.1f}s — new title: {new_title!r}", flush=True)
                    await asyncio.sleep(1)
                except Exception as e:
                    elapsed = time.monotonic() - t0
                    final_title = await page.title()
                    print(f"[CF] Challenge NOT resolved after {elapsed:.1f}s — title: {final_title!r} — {e}", flush=True)
    except Exception as e:
        print(f"[CF] Check failed: {e}", flush=True)


async def warmup(ctx: BrowserContext, url: str | None = None, timeout: int = 300_000) -> bool:
    """Ensure the context has a valid Cloudflare clearance.

    Opens the given URL (or SPOJ homepage) in a real page, waits through any
    challenge, and leaves the resulting ``cf_clearance`` cookie in the profile
    for later :func:`fetch_problem_body_http` calls.

    In headed mode, blocks indefinitely until the user solves the challenge.
    Returns True if the final page does not look like a CF interstitial.
    """
    target = url or f"{BASE_URL}/problems/classical/"
    page = await ctx.new_page()
    try:
        await page.goto(target, wait_until="domcontentloaded", timeout=60_000)
        await _wait_through_cloudflare(page, timeout=timeout)
        # Give CF cookie a beat to be set by the runtime script
        await asyncio.sleep(1.5)
        title = await page.title()
        if "Just a moment" in title or "Attention Required" in title:
            return False
        try:
            content = await page.content()
            return not looks_like_cf(content)
        except Exception:
            return True
    finally:
        await page.close()


async def has_cf_clearance(ctx: BrowserContext) -> bool:
    """Return True if the context has a non-empty ``cf_clearance`` cookie.

    Cheap presence check — does not actually hit the network. Use
    :func:`is_http_cleared` to verify the cookie works.
    """
    cookies = await ctx.cookies(BASE_URL)
    for c in cookies:
        if c.get("name") == "cf_clearance" and c.get("value"):
            return True
    return False


async def is_http_cleared(ctx: BrowserContext, probe_slug: str = "TEST") -> bool:
    """Probe the HTTP path to see whether ``ctx.request`` actually passes CF.

    Returns True on a 200 non-CF response for ``/problems/{probe_slug}/``.
    Used before a big batch so we can decide whether a real (page-based)
    warmup is needed.
    """
    try:
        resp = await ctx.request.get(
            f"{BASE_URL}/problems/{probe_slug}/",
            timeout=20_000,
        )
    except Exception:
        return False
    if resp.status != 200:
        return False
    try:
        html = await resp.text()
    except Exception:
        return False
    return not looks_like_cf(html)


async def fetch_problem_body(
    ctx: BrowserContext,
    slug: str,
    url: str | None = None,
) -> tuple[str, str]:
    """Page-based body fetch (slow, used when HTTP path hits CF).

    Returns (inner_html_of_problem_body, final_url).
    """
    target = url or f"{BASE_URL}/problems/{slug}/"
    page = await fetch_page(ctx, target)
    try:
        html = await page.evaluate("""() => {
            const el = document.querySelector('#problem-body');
            return el ? el.innerHTML : '';
        }""")
        final_url = page.url
        return html, final_url
    finally:
        await page.close()


async def fetch_problem_body_http(
    ctx: BrowserContext,
    slug: str,
    url: str | None = None,
    timeout: int = 30_000,
) -> tuple[str, str]:
    """Fast body fetch using ``ctx.request.get`` (no page render).

    Raises:
        CloudflareBlocked: if the response body looks like a CF challenge.
        RuntimeError: on HTTP error.
    """
    target = url or f"{BASE_URL}/problems/{slug}/"
    resp = await ctx.request.get(target, timeout=timeout)
    # 403/503 from SPOJ almost always means Cloudflare is blocking.
    if resp.status in (403, 503):
        raise CloudflareBlocked(f"HTTP {resp.status} (CF) for {slug}")
    if resp.status >= 400:
        raise RuntimeError(f"HTTP {resp.status} for {slug}")
    html = await resp.text()
    if looks_like_cf(html):
        raise CloudflareBlocked(f"CF challenge page for {slug}")
    body = extract_problem_body(html)
    return body, target


class CloudflareBlocked(RuntimeError):
    """Raised when a fetch response is a Cloudflare challenge page."""


async def fetch_index_page(
    ctx: BrowserContext,
    section: str,
    page_num: int,
) -> list[dict]:
    """Fetch one page of the SPOJ problem index.

    Returns list of dicts: slug, name, url, users, accepted, rank.
    """
    start = page_num * 50
    url = f"{BASE_URL}/problems/{section}/sort=0,start={start}"
    page = await fetch_page(ctx, url)

    try:
        try:
            await page.wait_for_selector("table.problems tbody tr", timeout=20_000)
        except Exception:
            return []

        problems = await page.evaluate("""() => {
            const rows = Array.from(document.querySelectorAll('table.problems tbody tr'));
            return rows.map((tr) => {
                const tds = tr.querySelectorAll('td');
                if (tds.length < 6) return null;
                const rankTxt = (tds[1].textContent || '').trim();
                const rank = parseInt(rankTxt, 10);
                if (!rank) return null;
                const a = tds[2].querySelector('a');
                if (!a) return null;
                const url = a.href || '';
                const m = url.match(/problems[/\\\\]([^/\\\\?]+)/i);
                const slug = m ? m[1].toUpperCase() : '';
                if (!slug) return null;
                const usersTxt = (tds[4].textContent || '0').replace(/,/g, '');
                const accTxt  = (tds[5].textContent || '0').replace(/,/g, '');
                return {
                    rank: rank,
                    slug: slug,
                    name: (a.textContent || '').trim(),
                    url: url.split('?')[0].replace(/\\/$/, '') + '/',
                    users: parseInt(usersTxt, 10) || 0,
                    accepted: parseFloat(accTxt) || 0,
                };
            }).filter(x => x && x.slug);
        }""")

        return problems
    finally:
        await page.close()


def html_to_markdown(html: str) -> str:
    """Convert problem body HTML to clean Markdown."""
    from markdownify import markdownify as md_conv

    if not html or not html.strip():
        return ""

    result = md_conv(
        html,
        heading_style="ATX",
        bullets="-",
        strip=["script", "style"],
        convert_links=True,
    )
    result = re.sub(r"\n{3,}", "\n\n", result)
    return result.strip()
