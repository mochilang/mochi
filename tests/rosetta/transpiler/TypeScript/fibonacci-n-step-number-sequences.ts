// Generated by Mochi v0.10.42 on 2025-07-28 07:51:38 GMT+7

function show(xs: number[]): string {
  let s: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    s = (s + _str(xs[i]));
    if ((i < ((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
      s = (s + " ");
    }
    i = (i + 1);
  }
  return s;
}
function gen(init: number[], n: number): number[] {
  let b: number[] = init;
  let res: number[] = [];
  let sum: number = 0;
  for (const x of b) {
    res.push(x);
    sum = (sum + x);
  }
  while (((Array.isArray(res) || typeof res === 'string' ? res.length : Object.keys(res ?? {}).length) < n)) {
    let next = sum;
    res.push(next);
    sum = ((sum + next) - b[Math.trunc(0)]);
    b = [...b.slice(1, (Array.isArray(b) || typeof b === 'string' ? b.length : Object.keys(b ?? {}).length)), next];
  }
  return res;
}
function main() {
  let n: number = 10;
  console.log(_str((" Fibonacci: " + show(gen([1, 1], n)))));
  console.log(_str(("Tribonacci: " + show(gen([1, 1, 2], n)))));
  console.log(_str(("Tetranacci: " + show(gen([1, 1, 2, 4], n)))));
  console.log(_str(("     Lucas: " + show(gen([2, 1], n)))));
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

