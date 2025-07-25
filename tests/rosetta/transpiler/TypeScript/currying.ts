// Generated by Mochi v0.10.40 on 2025-07-26 00:06:11 GMT+7

function Foo_Method(self: Foo, b: number): number {
  return (self.value + b);
}
export interface Foo { value: number }
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
  function pow(base: number, exp: number): number {
    let result: number = 1.0;
    let i: number = 0;
    while ((i < Math.trunc(exp))) {
      result = (result * base);
      i = (i + 1);
    }
    return result;
  }
  function PowN(b: number) {
    return (e) => pow(b, e);
  }
  function PowE(e: number) {
    return (b) => pow(b, e);
  }
  function main() {
    const pow2 = PowN(2.0);
    const cube = PowE(3.0);
    console.log(("2^8 = " + String(pow2(8.0))));
    console.log(("4³ = " + String(cube(4.0))));
    let a: Foo = {"value": 2, "Method": function(...args){ return Foo_Method(this, ...args); }};
    const fn1 = (b) => a.Method(b);
    const fn2 = (f, b) => f.Method(b);
    console.log(("2 + 2 = " + String(a.Method(2))));
    console.log(("2 + 3 = " + String(fn1(3))));
    console.log(("2 + 4 = " + String(fn2(a, 4))));
    console.log(("3 + 5 = " + String(fn2({"value": 3, "Method": function(...args){ return Foo_Method(this, ...args); }}, 5))));
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

