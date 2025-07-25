// Generated by Mochi v0.10.40 on 2025-07-26 00:06:00 GMT+7

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
  function pow_big(base: number, exp: number): number {
    let result: number = 1;
    let b: number = base;
    let e: number = exp;
    while ((e > 0)) {
      if (((e % 2) == 1)) {
        result = (result * b);
      }
      b = (b * b);
      e = Math.trunc(Math.trunc(e / 2));
    }
    return result;
  }
  function cullen(n: number): number {
    const two_n: number = pow_big(2, n);
    return ((two_n * n) + 1);
  }
  function woodall(n: number): number {
    return (cullen(n) - 2);
  }
  function show_list(xs: number[]): string {
    let line: string = "";
    let i: number = 0;
    while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
      line = (line + String(xs[i]));
      if ((i < ((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
        line = (line + " ");
      }
      i = (i + 1);
    }
    return line;
  }
  function main() {
    let cnums: number[] = [];
    let i: number = 1;
    while ((i <= 20)) {
      cnums = [...cnums, cullen(i)];
      i = (i + 1);
    }
    console.log("First 20 Cullen numbers (n * 2^n + 1):");
    console.log(show_list(cnums));
    let wnums: number[] = [];
    i = 1;
    while ((i <= 20)) {
      wnums = [...wnums, woodall(i)];
      i = (i + 1);
    }
    console.log("\nFirst 20 Woodall numbers (n * 2^n - 1):");
    console.log(show_list(wnums));
    const cprimes: number[] = [1, 141, 4713, 5795, 6611];
    console.log("\nFirst 5 Cullen primes (in terms of n):");
    console.log(show_list(cprimes));
    const wprimes: number[] = [2, 3, 6, 30, 75, 81, 115, 123, 249, 362, 384, 462];
    console.log("\nFirst 12 Woodall primes (in terms of n):");
    console.log(show_list(wprimes));
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

