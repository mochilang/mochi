// Generated by Mochi v0.10.40 on 2025-07-25 17:22:14 GMT+7

var _nowSeed = 0;
var _nowSeeded = false;
{
  let s = "";
  if (typeof Deno !== "undefined") {
    try {
      s = Deno.env.get("MOCHI_NOW_SEED") ?? "";
    } catch (_e) {
      s = "";
    }
  } else if (typeof process !== "undefined") {
    s = process.env.MOCHI_NOW_SEED || "";
  }
  if (s) {
    const v = parseInt(s, 10);
    if (!isNaN(v)) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
function _now(): number {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  if (typeof Deno !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  if (typeof performance !== 'undefined') {
    return Math.trunc(performance.now() * 1e6);
  }
  return Date.now() * 1000;
}
function _mem(): number {
  if (typeof Deno !== 'undefined') {
    return (Deno.memoryUsage?.().rss ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().rss;
  }
  return 0;
}
{
  const _startMem = _mem()
  const _start = _now()
  function poly(p: number): string {
    let s: string = "";
    let coef: number = 1;
    let i: number = p;
    if ((coef != 1)) {
      s = (s + String(coef));
    }
    while ((i > 0)) {
      s = (s + "x");
      if ((i != 1)) {
        s = ((s + "^") + String(i));
      }
      coef = Math.trunc(Math.trunc((coef * i) / ((p - i) + 1)));
      let d: number = coef;
      if ((((p - (i - 1)) % 2) == 1)) {
        d = -d;
      }
      if ((d < 0)) {
        s = ((s + " - ") + String(-d));
      } else {
        s = ((s + " + ") + String(d));
      }
      i = (i - 1);
    }
    if ((s == "")) {
      s = "1";
    }
    return s;
  }
  function aks(n: number): boolean {
    if ((n < 2)) {
      return false;
    }
    let c: number = n;
    let i: number = 1;
    while ((i < n)) {
      if (((c % n) != 0)) {
        return false;
      }
      c = Math.trunc(Math.trunc((c * (n - i)) / (i + 1)));
      i = (i + 1);
    }
    return true;
  }
  function main() {
    let p: number = 0;
    while ((p <= 7)) {
      console.log(((String(p) + ":  ") + poly(p)));
      p = (p + 1);
    }
    let first: boolean = true;
    p = 2;
    let line: string = "";
    while ((p < 50)) {
      if (aks(p)) {
        if (first) {
          line = (line + String(p));
          first = false;
        } else {
          line = ((line + " ") + String(p));
        }
      }
      p = (p + 1);
    }
    console.log(line);
  }
  main();
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  const _endMem = _mem()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
}

