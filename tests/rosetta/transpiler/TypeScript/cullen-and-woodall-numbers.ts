// Generated by Mochi v0.10.55 on 2025-08-02 20:30:48 GMT+7

function pow_big(base: number, exp: number): number {
  let result: number = 1n;
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
  let two_n: number = pow_big(2n, n);
  return (BigInt((BigInt(two_n) * BigInt(n))) + 1n);
}
function woodall(n: number): number {
  return (BigInt(cullen(n)) - 2n);
}
function show_list(xs: number[]): string {
  let line: string = "";
  let i: number = 0;
  while ((i < Number(Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    line = (line + _str(xs[i]));
    if ((i < (Number(Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
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
    cnums.push(cullen(i));
    i = (i + 1);
  }
  console.log(_str("First 20 Cullen numbers (n * 2^n + 1):"));
  console.log(_str(show_list(cnums)));
  let wnums: number[] = [];
  i = 1;
  while ((i <= 20)) {
    wnums.push(woodall(i));
    i = (i + 1);
  }
  console.log(_str("\nFirst 20 Woodall numbers (n * 2^n - 1):"));
  console.log(_str(show_list(wnums)));
  let cprimes: number[] = [1n, 141n, 4713n, 5795n, 6611n];
  console.log(_str("\nFirst 5 Cullen primes (in terms of n):"));
  console.log(_str(show_list(cprimes)));
  let wprimes: number[] = [2n, 3n, 6n, 30n, 75n, 81n, 115n, 123n, 249n, 362n, 384n, 462n];
  console.log(_str("\nFirst 12 Woodall primes (in terms of n):"));
  console.log(_str(show_list(wprimes)));
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

