// Generated by Mochi v0.10.42 on 2025-07-27 20:33:31 GMT+7

function mod(n: number, m: number): number {
  return (((n % m) + m) % m);
}
function isPrime(n: number): boolean {
  if ((n < 2)) {
    return false;
  }
  if (((n % 2) == 0)) {
    return (n == 2);
  }
  if (((n % 3) == 0)) {
    return (n == 3);
  }
  let d: number = 5;
  while (((d * d) <= n)) {
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 2);
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 4);
  }
  return true;
}
function pad(n: number, width: number): string {
  let s: string = String(n);
  while (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) < width)) {
    s = (" " + s);
  }
  return s;
}
function carmichael(p1: number) {
  for (let h3 = 2; h3 < p1; h3++) {
    for (let d = 1; d < (h3 + p1); d++) {
      if ((((((h3 + p1) * (p1 - 1)) % d) == 0) && (mod((-p1 * p1), h3) == (d % h3)))) {
        let p2: number = (1 + Math.trunc(((p1 - 1) * (h3 + p1)) / d));
        if (!isPrime(p2)) {
          continue
        }
        let p3: number = (1 + Math.trunc((p1 * p2) / h3));
        if (!isPrime(p3)) {
          continue
        }
        if ((((p2 * p3) % (p1 - 1)) != 1)) {
          continue
        }
        let c: number = ((p1 * p2) * p3);
        console.log(((((((pad(p1, 2) + "   ") + pad(p2, 4)) + "   ") + pad(p3, 5)) + "     ") + String(c)));
      }
    }
  }
}
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
(() => {
  const _startMem = _mem()
  const _start = _now()
  console.log("The following are Carmichael munbers for p1 <= 61:\n");
  console.log("p1     p2      p3     product");
  console.log("==     ==      ==     =======");
  for (let p1 = 2; p1 < 62; p1++) {
    if (isPrime(p1)) {
      carmichael(p1);
    }
  }
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
})();

