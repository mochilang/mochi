// Generated by Mochi v0.10.55 on 2025-08-02 20:23:43 GMT+7

function main() {
  let row: number = 3;
  let col: number = 4;
  let a: number[][] = [];
  let i: number = 0;
  while ((i < row)) {
    let rowArr: number[] = [];
    let j: number = 0;
    while ((j < col)) {
      rowArr.push(0);
      j = (j + 1);
    }
    a.push(rowArr);
    i = (i + 1);
  }
  console.log(_str(("a[0][0] = " + _str(a[Math.trunc(0)][Math.trunc(0)]))));
  a[Math.trunc((row - 1))][Math.trunc((col - 1))] = 7;
  console.log(_str(((((("a[" + _str((row - 1))) + "][") + _str((col - 1))) + "] = ") + _str(a[Math.trunc(Math.trunc((row - 1)))][Math.trunc(Math.trunc((col - 1)))]))));
  a = null;
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
    return (Deno.memoryUsage?.().heapUsed ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().heapUsed;
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
  globalThis.gc?.()
  const _startMem = _mem()
  const _start = _now()
  main();
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  const _endMem = _mem()
  globalThis.gc?.()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

