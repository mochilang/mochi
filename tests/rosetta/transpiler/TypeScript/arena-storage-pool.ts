// Generated by Mochi v0.10.42 on 2025-07-27 18:45:14 GMT+7

function poolPut(p: number[], x: number): number[] {
  return [...p, x];
}
function poolGet(p: number[]): Record<string, any> {
  if (((Array.isArray(p) || typeof p === 'string' ? p.length : Object.keys(p ?? {}).length) == 0)) {
    console.log("pool empty");
    return {"pool": p, "val": 0};
  }
  let idx: number = ((Array.isArray(p) || typeof p === 'string' ? p.length : Object.keys(p ?? {}).length) - 1);
  let v: number = p[idx];
  p = p.slice(0, idx);
  return {"pool": p, "val": v};
}
function clearPool(p: number[]): number[] {
  return [];
}
function main() {
  let pool: number[] = [];
  let i: number = 1;
  let j: number = 2;
  console.log(String((i + j)));
  pool = poolPut(pool, i);
  pool = poolPut(pool, j);
  i = 0;
  j = 0;
  let res1: Record<string, any> = poolGet(pool);
  pool = res1.pool;
  i = Math.trunc(res1.val);
  let res2: Record<string, any> = poolGet(pool);
  pool = res2.pool;
  j = Math.trunc(res2.val);
  i = 4;
  j = 5;
  console.log(String((i + j)));
  pool = poolPut(pool, i);
  pool = poolPut(pool, j);
  i = 0;
  j = 0;
  pool = clearPool(pool);
  let res3: Record<string, any> = poolGet(pool);
  pool = res3.pool;
  i = Math.trunc(res3.val);
  let res4: Record<string, any> = poolGet(pool);
  pool = res4.pool;
  j = Math.trunc(res4.val);
  i = 7;
  j = 8;
  console.log(String((i + j)));
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

