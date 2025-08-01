// Generated by Mochi v0.10.42 on 2025-07-27 21:44:59 GMT+7

function listStr(xs: number[]): string {
  let s: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    s = (s + String(xs[i]));
    if ((i < ((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
      s = (s + " ");
    }
    i = (i + 1);
  }
  s = (s + "]");
  return s;
}
function ordered(xs: number[]): boolean {
  if (((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) == 0)) {
    return true;
  }
  let prev: number = xs[Math.trunc(0)];
  let i: number = 1;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    if ((xs[i] < prev)) {
      return false;
    }
    prev = xs[i];
    i = (i + 1);
  }
  return true;
}
function outOfOrder(n: number): number[] {
  if ((n < 2)) {
    return [];
  }
  let r: number[] = [];
  while (true) {
    r = [];
    let i: number = 0;
    while ((i < n)) {
      r.push((_now() % 3));
      i = (i + 1);
    }
    if (!ordered(r)) {
      break
    }
  }
  return r;
}
function sort3(a: number[]): number[] {
  let lo: number = 0;
  let mid: number = 0;
  let hi: number = ((Array.isArray(a) || typeof a === 'string' ? a.length : Object.keys(a ?? {}).length) - 1);
  while ((mid <= hi)) {
    let v: number = a[mid];
    if ((v == 0)) {
      let tmp: number = a[lo];
      a[lo] = a[mid];
      a[mid] = tmp;
      lo = (lo + 1);
      mid = (mid + 1);
    } else {
      if ((v == 1)) {
        mid = (mid + 1);
      } else {
        let tmp: number = a[mid];
        a[mid] = a[hi];
        a[hi] = tmp;
        hi = (hi - 1);
      }
    }
  }
  return a;
}
function main() {
  let f: number[] = outOfOrder(12);
  console.log(listStr(f));
  f = sort3(f);
  console.log(listStr(f));
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
})();

