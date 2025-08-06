function backtrack(candidates: number[], start: number, target: number, path: number[], result: number[][]): number[][] {
  if ((target == 0)) {
    return [...result, path];
  }
  let i: number = start;
  while ((i < Number(Array.isArray(candidates) || typeof candidates === 'string' ? candidates.length : Object.keys(candidates ?? {}).length))) {
    let value: number = candidates[i];
    if ((value <= target)) {
      let new_path: any[] = [...path, value];
      result = backtrack(candidates, i, (target - value), new_path, result);
    }
    i = (i + 1);
  }
  return result;
}
function combination_sum(candidates: number[], target: number): number[][] {
  let path: number[] = [];
  let result: number[][] = [];
  return backtrack(candidates, 0, target, path, result);
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
  console.log(_str(_str(combination_sum([2, 3, 5], 8))));
  console.log(_str(_str(combination_sum([2, 3, 6, 7], 7))));
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

