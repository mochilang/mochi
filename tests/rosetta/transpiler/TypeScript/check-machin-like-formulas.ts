// Generated by Mochi v0.10.42 on 2025-07-27 20:33:56 GMT+7

function br(n: number, d: number): number {
  return (n / d);
}
let testCases: Record<string, number>[][] = [[{"a": 1, "n": 1, "d": 2}, {"a": 1, "n": 1, "d": 3}], [{"a": 2, "n": 1, "d": 3}, {"a": 1, "n": 1, "d": 7}], [{"a": 4, "n": 1, "d": 5}, {"a": -1, "n": 1, "d": 239}], [{"a": 5, "n": 1, "d": 7}, {"a": 2, "n": 3, "d": 79}], [{"a": 1, "n": 1, "d": 2}, {"a": 1, "n": 1, "d": 5}, {"a": 1, "n": 1, "d": 8}], [{"a": 4, "n": 1, "d": 5}, {"a": -1, "n": 1, "d": 70}, {"a": 1, "n": 1, "d": 99}], [{"a": 5, "n": 1, "d": 7}, {"a": 4, "n": 1, "d": 53}, {"a": 2, "n": 1, "d": 4443}], [{"a": 6, "n": 1, "d": 8}, {"a": 2, "n": 1, "d": 57}, {"a": 1, "n": 1, "d": 239}], [{"a": 8, "n": 1, "d": 10}, {"a": -1, "n": 1, "d": 239}, {"a": -4, "n": 1, "d": 515}], [{"a": 12, "n": 1, "d": 18}, {"a": 8, "n": 1, "d": 57}, {"a": -5, "n": 1, "d": 239}], [{"a": 16, "n": 1, "d": 21}, {"a": 3, "n": 1, "d": 239}, {"a": 4, "n": 3, "d": 1042}], [{"a": 22, "n": 1, "d": 28}, {"a": 2, "n": 1, "d": 443}, {"a": -5, "n": 1, "d": 1393}, {"a": -10, "n": 1, "d": 11018}], [{"a": 22, "n": 1, "d": 38}, {"a": 17, "n": 7, "d": 601}, {"a": 10, "n": 7, "d": 8149}], [{"a": 44, "n": 1, "d": 57}, {"a": 7, "n": 1, "d": 239}, {"a": -12, "n": 1, "d": 682}, {"a": 24, "n": 1, "d": 12943}], [{"a": 88, "n": 1, "d": 172}, {"a": 51, "n": 1, "d": 239}, {"a": 32, "n": 1, "d": 682}, {"a": 44, "n": 1, "d": 5357}, {"a": 68, "n": 1, "d": 12943}], [{"a": 88, "n": 1, "d": 172}, {"a": 51, "n": 1, "d": 239}, {"a": 32, "n": 1, "d": 682}, {"a": 44, "n": 1, "d": 5357}, {"a": 68, "n": 1, "d": 12944}]];
function format(ts: Record<string, number>[]): string {
  let s: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(ts) || typeof ts === 'string' ? ts.length : Object.keys(ts ?? {}).length))) {
    let t: Record<string, number> = ts[i];
    s = (((((((s + "{") + String(t.a)) + " ") + String(t.n)) + " ") + String(t.d)) + "}");
    if ((i < ((Array.isArray(ts) || typeof ts === 'string' ? ts.length : Object.keys(ts ?? {}).length) - 1))) {
      s = (s + " ");
    }
    i = (i + 1);
  }
  return (s + "]");
}
function tanEval(coef: number, f: number): number {
  if ((coef == 1)) {
    return f;
  }
  if ((coef < 0)) {
    return -tanEval(-coef, f);
  }
  let ca: number = Math.trunc(coef / 2);
  let cb: number = (coef - ca);
  let a: number = tanEval(ca, f);
  let b: number = tanEval(cb, f);
  return ((a + b) / (1 - (a * b)));
}
function tans(m: Record<string, number>[]): number {
  if (((Array.isArray(m) || typeof m === 'string' ? m.length : Object.keys(m ?? {}).length) == 1)) {
    let t: Record<string, number> = m[Math.trunc(0)];
    return tanEval(t.a, br(t.n, t.d));
  }
  let half: number = Math.trunc((Array.isArray(m) || typeof m === 'string' ? m.length : Object.keys(m ?? {}).length) / 2);
  let a: number = tans(m.slice(0, half));
  let b: number = tans(m.slice(half));
  return ((a + b) / (1 - (a * b)));
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
  for (const ts of testCases) {
    console.log(((("tan " + format(ts)) + " = ") + String(tans(ts))));
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

