// Generated by Mochi v0.10.42 on 2025-07-28 01:01:01 GMT+7

function printFactors(n: number) {
  if ((n < 1)) {
    console.log(_str((("\nFactors of " + _str(n)) + " not computed")));
    return;
  }
  console.log(_str((("\nFactors of " + _str(n)) + ": ")));
  let fs: number[] = [1];
  function apf(p: number, e: number) {
    let orig: number = (Array.isArray(fs) || typeof fs === 'string' ? fs.length : Object.keys(fs ?? {}).length);
    let pp: number = p;
    let i: number = 0;
    while ((i < e)) {
      let j: number = 0;
      while ((j < orig)) {
        fs.push((fs[j] * pp));
        j = (j + 1);
      }
      i = (i + 1);
      pp = (pp * p);
    }
  }
  let e: number = 0;
  let m: number = n;
  while (((m % 2) == 0)) {
    m = Math.trunc(Math.trunc(m / 2));
    e = (e + 1);
  }
  apf(2, e);
  let d: number = 3;
  while ((m > 1)) {
    if (((d * d) > m)) {
      d = m;
    }
    e = 0;
    while (((m % d) == 0)) {
      m = Math.trunc(Math.trunc(m / d));
      e = (e + 1);
    }
    if ((e > 0)) {
      apf(d, e);
    }
    d = (d + 2);
  }
  console.log(_str(_str(fs)));
  console.log(_str(("Number of factors = " + _str((Array.isArray(fs) || typeof fs === 'string' ? fs.length : Object.keys(fs ?? {}).length)))));
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
  printFactors(-1);
  printFactors(0);
  printFactors(1);
  printFactors(2);
  printFactors(3);
  printFactors(53);
  printFactors(45);
  printFactors(64);
  printFactors(600851475143);
  printFactors(999999999999999989);
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

