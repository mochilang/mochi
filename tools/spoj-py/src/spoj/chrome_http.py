"""Chrome-TLS HTTP backend for SPOJ fetching.

Uses `curl_cffi` (BoringSSL, Chrome TLS fingerprint) with cookies pulled from
the user's real Chrome. This combination is currently the reliable way to
bypass Cloudflare's bot checks for SPOJ: Patchright's automation-patched
Chromium is still detectable, but the user's real Chrome has already solved
CF, and curl_cffi replays the same TLS handshake + cookies.

Usage (async):

    client = ChromeClient()
    body, url = await client.fetch_body("TEST")
    await client.close()
"""

from __future__ import annotations

import asyncio
from dataclasses import dataclass, field
from typing import Any

from curl_cffi.requests import AsyncSession

from spoj.browser import (
    BASE_URL,
    CloudflareBlocked,
    extract_problem_body,
    looks_like_cf,
)

# Matches user-agent emitted by curl_cffi's chrome146 profile.
_DEFAULT_HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/147.0.0.0 Safari/537.36"
    ),
    "Accept": (
        "text/html,application/xhtml+xml,application/xml;q=0.9,"
        "image/avif,image/webp,*/*;q=0.8"
    ),
    "Accept-Language": "en-US,en;q=0.9",
    "Accept-Encoding": "gzip, deflate, br",
    "Upgrade-Insecure-Requests": "1",
    "Sec-Fetch-Dest": "document",
    "Sec-Fetch-Mode": "navigate",
    "Sec-Fetch-Site": "none",
    "Sec-Fetch-User": "?1",
    "Referer": f"{BASE_URL}/",
}

# curl_cffi impersonation profiles, newest first. We fall back to older ones
# if the newest is rejected (e.g. CF updates its JA4 hash and the freshest
# profile hasn't been released yet).
_PROFILES_NEWEST_FIRST = (
    "chrome146",
    "chrome145",
    "chrome142",
    "chrome136",
    "chrome133a",
    "chrome131",
)


def load_chrome_cookies(
    domain: str = "spoj.com",
    browser: str = "chrome",
) -> dict[str, str]:
    """Pull cookies for ``domain`` from the user's real browser.

    Supports chrome (default), firefox, brave, edge, safari, chromium, opera.
    Raises RuntimeError with a helpful message on failure.
    """
    import browser_cookie3

    fn = getattr(browser_cookie3, browser, None)
    if fn is None:
        raise RuntimeError(
            f"Unknown browser {browser!r}. "
            "Supported: chrome, firefox, brave, edge, safari, chromium, opera."
        )
    try:
        cj = fn(domain_name=domain)
    except Exception as exc:
        raise RuntimeError(
            f"Failed to read cookies from {browser}: {exc}. "
            "Quit the browser and retry, or use a different --cookies-browser."
        ) from exc
    cookies = {c.name: c.value for c in cj if c.value}
    if "cf_clearance" not in cookies:
        raise RuntimeError(
            f"No cf_clearance cookie found in {browser} for {domain}. "
            f"Open {BASE_URL}/problems/classical/ in that browser, solve "
            "the Cloudflare challenge once, then retry."
        )
    return cookies


@dataclass
class ChromeClient:
    """Thin wrapper around AsyncSession preloaded with Chrome cookies.

    One instance is reused across many fetches (connection pooling). Call
    :meth:`close` when done.
    """

    impersonate: str = "chrome146"
    cookies_browser: str = "chrome"
    cookie_domain: str = "spoj.com"
    timeout: float = 30.0
    headers: dict[str, str] = field(default_factory=lambda: dict(_DEFAULT_HEADERS))
    _session: AsyncSession | None = None
    _cookies: dict[str, str] = field(default_factory=dict)

    def __post_init__(self) -> None:
        self._cookies = load_chrome_cookies(self.cookie_domain, self.cookies_browser)
        self._session = AsyncSession(
            impersonate=self.impersonate,
            headers=self.headers,
            cookies=self._cookies,
            timeout=self.timeout,
        )

    async def close(self) -> None:
        if self._session is not None:
            await self._session.close()
            self._session = None

    async def __aenter__(self) -> "ChromeClient":
        return self

    async def __aexit__(self, *exc: Any) -> None:
        await self.close()

    def refresh_cookies(self) -> None:
        """Re-read cookies from the browser DB (e.g. after user re-solves CF)."""
        self._cookies = load_chrome_cookies(self.cookie_domain, self.cookies_browser)
        if self._session is not None:
            self._session.cookies.update(self._cookies)

    async def fetch_body(self, slug: str, url: str | None = None) -> tuple[str, str]:
        """Fetch a problem body. Returns (inner_html, final_url).

        Raises :class:`CloudflareBlocked` on CF challenge or 403/503.
        Raises :class:`RuntimeError` on other HTTP errors.
        """
        assert self._session is not None, "ChromeClient closed"
        target = url or f"{BASE_URL}/problems/{slug}/"

        last_exc: Exception | None = None
        for profile in self._profiles():
            try:
                r = await self._session.get(target, impersonate=profile)
            except Exception as exc:
                last_exc = exc
                continue

            if r.status_code in (403, 503):
                last_exc = CloudflareBlocked(
                    f"HTTP {r.status_code} (CF) for {slug} [profile={profile}]"
                )
                continue
            if r.status_code >= 400:
                raise RuntimeError(f"HTTP {r.status_code} for {slug}")

            text = r.text
            if looks_like_cf(text):
                last_exc = CloudflareBlocked(f"CF challenge page for {slug}")
                continue

            body = extract_problem_body(text)
            return body, target

        if last_exc:
            raise last_exc
        raise RuntimeError(f"All impersonation profiles failed for {slug}")

    def _profiles(self) -> tuple[str, ...]:
        """Profiles to try, with the configured one first."""
        primary = self.impersonate
        rest = tuple(p for p in _PROFILES_NEWEST_FIRST if p != primary)
        return (primary,) + rest


async def probe(
    client: ChromeClient | None = None,
    slug: str = "TEST",
) -> bool:
    """Return True if the Chrome backend can fetch a problem body right now."""
    own = client is None
    if own:
        client = ChromeClient()
    try:
        body, _ = await client.fetch_body(slug)
        return bool(body)
    except Exception:
        return False
    finally:
        if own and client is not None:
            await client.close()
