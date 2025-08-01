// Generated by Mochi v0.10.55 on 2025-08-02 21:02:16 GMT+7

export interface MDRResult { mp: number; mdr: number }
function pad(s: string, width: number): string {
  let out: string = s;
  while ((Number(Array.isArray(out) || typeof out === 'string' ? out.length : Object.keys(out ?? {}).length) < width)) {
    out = (" " + out);
  }
  return out;
}
function mult(n: bigint, base: number): bigint {
  let m: bigint = 1n;
  let x: bigint = BigInt(n);
  let b: bigint = BigInt(base);
  while ((BigInt(x) > 0n)) {
    m = (m * (x % b));
    x = x / b;
  }
  return m;
}
function multDigitalRoot(n: bigint, base: number): MDRResult {
  let m: bigint = BigInt(n);
  let mp: number = 0;
  let b: bigint = BigInt(base);
  while ((m >= b)) {
    m = mult(m, base);
    mp = (mp + 1);
  }
  return {mp, "mdr": Number(m)};
}
function main() {
  let base: number = 10;
  let size: number = 5;
  console.log(_str(((((pad("Number", 20) + " ") + pad("MDR", 3)) + " ") + pad("MP", 3))));
  let nums: bigint[] = [123321n, 7739n, 893n, 899998n, 3778888999n, 277777788888899n];
  let i: number = 0;
  while ((i < Number(Array.isArray(nums) || typeof nums === 'string' ? nums.length : Object.keys(nums ?? {}).length))) {
    let n: bigint = BigInt(nums[i]);
    let r: MDRResult = multDigitalRoot(n, base);
    console.log(_str(((((pad(_str(n), 20) + " ") + pad(_str(r.mdr), 3)) + " ") + pad(_str(r.mp), 3))));
    i = (i + 1);
  }
  console.log(_str(""));
  let list: number[][] = [];
  let idx: number = 0;
  while ((idx < base)) {
    list.push([]);
    idx = (idx + 1);
  }
  let cnt: bigint = BigInt((size * base));
  let n: bigint = 0n;
  let b: bigint = BigInt(base);
  while ((cnt > BigInt(0))) {
    let r: MDRResult = multDigitalRoot(n, base);
    let mdr: number = r.mdr;
    if ((Number(Array.isArray(list[mdr]) || typeof list[mdr] === 'string' ? list[mdr].length : Object.keys(list[mdr] ?? {}).length) < size)) {
      list[mdr] = [...list[mdr], Number(n)];
      cnt = (cnt - BigInt(1));
    }
    n = (BigInt(n) + 1n);
  }
  console.log(_str("MDR: First"));
  let j: number = 0;
  while ((j < base)) {
    console.log(_str(((pad(_str(j), 3) + ": ") + _str(list[j]))));
    j = (j + 1);
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
  globalThis.gc?.()
  const _endMem = _mem()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

