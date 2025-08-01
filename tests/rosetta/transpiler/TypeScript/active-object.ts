// Generated by Mochi v0.10.42 on 2025-07-28 10:40:04 GMT+7

let PI: number = 3.141592653589793;
function sinApprox(x: number): number {
  let term: number = x;
  let sum: number = x;
  let n: number = 1;
  while ((n <= 12)) {
    let denom = ((2 * n) * ((2 * n) + 1));
    term = (((-term * x) * x) / denom);
    sum = (sum + term);
    n = (n + 1);
  }
  return sum;
}
let dt: number = 0.01;
let s: number = 0.0;
let t1: number = 0.0;
let i: number = 1;
let i2: number = 1;
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
function _str(x: any): string {
  if (typeof x === 'number') {
    if (Object.is(x, -0)) return '-0';
    if (x === Infinity) return '+Inf';
    if (x === -Infinity) return '-Inf';
    if (Number.isNaN(x)) return 'NaN';
  }
  return String(x);
}
(() => {
  const _startMem = _mem()
  const _start = _now()
  let k1: number = sinApprox(0.0);
  while ((i <= 200)) {
    let t2: number = (i * dt);
    let k2: number = sinApprox((t2 * PI));
    s = (s + (((k1 + k2) * 0.5) * (t2 - t1)));
    t1 = t2;
    k1 = k2;
    i = (i + 1);
  }
  while ((i2 <= 50)) {
    let t2: number = (2.0 + (i2 * dt));
    let k2: number = 0.0;
    s = (s + (((k1 + k2) * 0.5) * (t2 - t1)));
    t1 = t2;
    k1 = k2;
    i2 = (i2 + 1);
  }
  console.log(_str(s));
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

