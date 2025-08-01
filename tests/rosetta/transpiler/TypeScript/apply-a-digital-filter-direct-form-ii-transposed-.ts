// Generated by Mochi v0.10.42 on 2025-07-27 18:45:08 GMT+7

function applyFilter(input: number[], a: number[], b: number[]): number[] {
  let out: number[] = [];
  let scale: number = (1.0 / a[Math.trunc(0)]);
  let i: number = 0;
  while ((i < (Array.isArray(input) || typeof input === 'string' ? input.length : Object.keys(input ?? {}).length))) {
    let tmp: number = 0.0;
    let j: number = 0;
    while (((j <= i) && (j < (Array.isArray(b) || typeof b === 'string' ? b.length : Object.keys(b ?? {}).length)))) {
      tmp = (tmp + (b[j] * input[Math.trunc((i - j))]));
      j = (j + 1);
    }
    j = 0;
    while (((j < i) && ((j + 1) < (Array.isArray(a) || typeof a === 'string' ? a.length : Object.keys(a ?? {}).length)))) {
      tmp = (tmp - (a[Math.trunc((j + 1))] * out[Math.trunc(((i - j) - 1))]));
      j = (j + 1);
    }
    out.push((tmp * scale));
    i = (i + 1);
  }
  return out;
}
let a: number[] = [1.0, -2.7756e-16, 0.33333333, -1.85e-17];
let b: number[] = [0.16666667, 0.5, 0.5, 0.16666667];
let sig: number[] = [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];
let res: number[] = applyFilter(sig, a, b);
let k: number = 0;
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
  while ((k < (Array.isArray(res) || typeof res === 'string' ? res.length : Object.keys(res ?? {}).length))) {
    console.log(res[k]);
    k = (k + 1);
  }
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

