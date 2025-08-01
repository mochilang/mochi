// Generated by Mochi v0.10.42 on 2025-07-27 21:45:03 GMT+7

function pow10(n: number): number {
  let r: number = 1.0;
  let i: number = 0;
  while ((i < n)) {
    r = (r * 10.0);
    i = (i + 1);
  }
  return r;
}
function powf(base: number, exp: number): number {
  if ((exp == 0.5)) {
    let guess: number = base;
    let i: number = 0;
    while ((i < 20)) {
      guess = ((guess + (base / guess)) / 2.0);
      i = (i + 1);
    }
    return guess;
  }
  let result: number = 1.0;
  let n: number = Math.trunc(exp);
  let i: number = 0;
  while ((i < n)) {
    result = (result * base);
    i = (i + 1);
  }
  return result;
}
function formatFloat(f: number, prec: number): string {
  let scale: number = pow10(prec);
  let scaled: number = ((f * scale) + 0.5);
  let n: number = Math.trunc(scaled);
  let digits: string = String(n);
  while (((Array.isArray(digits) || typeof digits === 'string' ? digits.length : Object.keys(digits ?? {}).length) <= prec)) {
    digits = ("0" + digits);
  }
  let intPart: string = (digits).substring(0, ((Array.isArray(digits) || typeof digits === 'string' ? digits.length : Object.keys(digits ?? {}).length) - prec));
  let fracPart: string = (digits).substring(((Array.isArray(digits) || typeof digits === 'string' ? digits.length : Object.keys(digits ?? {}).length) - prec), (Array.isArray(digits) || typeof digits === 'string' ? digits.length : Object.keys(digits ?? {}).length));
  return ((intPart + ".") + fracPart);
}
function padLeft(s: string, w: number): string {
  let res: string = "";
  let n: number = (w - (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length));
  while ((n > 0)) {
    res = (res + " ");
    n = (n - 1);
  }
  return (res + s);
}
function rowString(row: number[]): string {
  let s: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(row) || typeof row === 'string' ? row.length : Object.keys(row ?? {}).length))) {
    s = (s + padLeft(formatFloat(row[i], 3), 6));
    if ((i < ((Array.isArray(row) || typeof row === 'string' ? row.length : Object.keys(row ?? {}).length) - 1))) {
      s = (s + " ");
    }
    i = (i + 1);
  }
  return (s + "] ");
}
function printMatrix(heading: string, m: number[][]) {
  console.log(heading);
  let i: number = 0;
  while ((i < (Array.isArray(m) || typeof m === 'string' ? m.length : Object.keys(m ?? {}).length))) {
    console.log(rowString(m[i]));
    i = (i + 1);
  }
}
function elementWiseMM(m1: number[][], m2: number[][], f): number[][] {
  let z: number[][] = [];
  let r: number = 0;
  while ((r < (Array.isArray(m1) || typeof m1 === 'string' ? m1.length : Object.keys(m1 ?? {}).length))) {
    let row: number[] = [];
    let c: number = 0;
    while ((c < (Array.isArray(m1[r]) || typeof m1[r] === 'string' ? m1[r].length : Object.keys(m1[r] ?? {}).length))) {
      row.push(f(m1[r][c], m2[r][c]));
      c = (c + 1);
    }
    z.push(row);
    r = (r + 1);
  }
  return z;
}
function elementWiseMS(m: number[][], s: number, f): number[][] {
  let z: number[][] = [];
  let r: number = 0;
  while ((r < (Array.isArray(m) || typeof m === 'string' ? m.length : Object.keys(m ?? {}).length))) {
    let row: number[] = [];
    let c: number = 0;
    while ((c < (Array.isArray(m[r]) || typeof m[r] === 'string' ? m[r].length : Object.keys(m[r] ?? {}).length))) {
      row.push(f(m[r][c], s));
      c = (c + 1);
    }
    z.push(row);
    r = (r + 1);
  }
  return z;
}
function add(a: number, b: number): number {
  return (a + b);
}
function sub(a: number, b: number): number {
  return (a - b);
}
function mul(a: number, b: number): number {
  return (a * b);
}
function div(a: number, b: number): number {
  return (a / b);
}
function exp(a: number, b: number): number {
  return powf(a, b);
}
function main() {
  let m1: number[][] = [[3.0, 1.0, 4.0], [1.0, 5.0, 9.0]];
  let m2: number[][] = [[2.0, 7.0, 1.0], [8.0, 2.0, 8.0]];
  printMatrix("m1:", m1);
  printMatrix("m2:", m2);
  console.log("");
  printMatrix("m1 + m2:", elementWiseMM(m1, m2, add));
  printMatrix("m1 - m2:", elementWiseMM(m1, m2, sub));
  printMatrix("m1 * m2:", elementWiseMM(m1, m2, mul));
  printMatrix("m1 / m2:", elementWiseMM(m1, m2, div));
  printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, exp));
  console.log("");
  let s: number = 0.5;
  console.log(("s: " + String(s)));
  printMatrix("m1 + s:", elementWiseMS(m1, s, add));
  printMatrix("m1 - s:", elementWiseMS(m1, s, sub));
  printMatrix("m1 * s:", elementWiseMS(m1, s, mul));
  printMatrix("m1 / s:", elementWiseMS(m1, s, div));
  printMatrix("m1 ^ s:", elementWiseMS(m1, s, exp));
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

