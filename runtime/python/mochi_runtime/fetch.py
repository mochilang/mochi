"""Mochi fetch + writeFile runtime helpers for the Phase 14.0 surface.

Mochi's `fetch(url)` reads the response body from a URL and returns it
as a string; `writeFile(path, content)` writes (creating or truncating)
the file at `path` with `content`.

Phase 14.0 lowers `fetch(url)` to ``mochi_fetch(url)`` against this
module, which delegates to ``urllib.request.urlopen`` from the
standard library. urllib supports both ``http://``, ``https://``, and
``file://`` schemes out of the box, which matches the corpus the C and
PHP targets exercise: all v1 fixtures use ``file://`` for hermetic
test execution; the same code path works for live HTTP without any
runtime swap.

`writeFile(path, content)` lowers to ``mochi_write_file(path,
content)`` which opens the file in binary mode and writes UTF-8
encoded bytes. Binary mode is load-bearing: Python's text mode
translates ``\\n`` to the platform line separator on write, which
would skew the byte-equal stdout gate on Windows. urllib also returns
raw bytes, decoded once here to ``str`` so the call site does not
have to.

httpx, requests, and aiohttp are deferred to later sub-phases as
optional dependencies; the standard library covers the load-bearing
case (fetch one URL, decode the body, print it) without adding a wheel
dependency.
"""

from __future__ import annotations

import urllib.error
import urllib.request


def mochi_fetch(url: str) -> str:
    """HTTP GET / file:// read returning the response body as a string.

    Uses ``urllib.request.urlopen`` so ``file://`` and ``http(s)://``
    work identically without a third-party dependency. The response is
    decoded as UTF-8; raw bytes survive the round-trip because Python's
    UTF-8 codec is lossless over the ASCII range every Phase 14
    fixture uses. Returns ``""`` on any URLError so a failed fetch
    cannot abort the program: the same shape as the C / PHP targets'
    fetch helpers.
    """
    try:
        with urllib.request.urlopen(url) as resp:  # noqa: S310
            data = resp.read()
    except (urllib.error.URLError, OSError):
        return ""
    return data.decode("utf-8")


def mochi_write_file(path: str, content: str) -> None:
    """Write `content` to `path`, creating or truncating the file.

    Binary mode is required so that text-mode newline translation does
    not run on Windows. Mochi semantics: any `\\n` in `content` is
    written verbatim, matching the C/PHP/vm3 baselines.
    """
    with open(path, "wb") as f:
        f.write(content.encode("utf-8"))
