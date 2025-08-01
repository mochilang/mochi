// Generated by Mochi v0.10.42 on 2025-07-27 21:21:19 GMT+7

export interface cds { i: number; s: string; b: number[]; m: Record<number, boolean> }
function copyList(src: number[]): number[] {
  let out: number[] = [];
  for (const v of src) {
    out.push(v);
  }
  return out;
}
function copyMap(src: Record<number, boolean>): Record<number, boolean> {
  let out: Record<number, boolean> = {};
  for (const k in src) {
    out[k] = src[k];
  }
  return out;
}
function deepcopy(c: cds): cds {
  return {"i": c.i, "s": c.s, "b": copyList(c.b), "m": copyMap(c.m)};
}
function cdsStr(c: cds): string {
  let bs: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(c.b) || typeof c.b === 'string' ? c.b.length : Object.keys(c.b ?? {}).length))) {
    bs = (bs + String(c.b[i]));
    if ((i < ((Array.isArray(c.b) || typeof c.b === 'string' ? c.b.length : Object.keys(c.b ?? {}).length) - 1))) {
      bs = (bs + " ");
    }
    i = (i + 1);
  }
  bs = (bs + "]");
  let ms: string = "map[";
  let first: boolean = true;
  for (const k in c.m) {
    if (!first) {
      ms = (ms + " ");
    }
    ms = (((ms + String(k)) + ":") + String(c.m[k]));
    first = false;
  }
  ms = (ms + "]");
  return (((((((("{" + String(c.i)) + " ") + c.s) + " ") + bs) + " ") + ms) + "}");
}
let c1: cds = {"i": 1, "s": "one", "b": [117, 110, 105, 116], "m": {[1]: true}};
let c2: cds = deepcopy(c1);
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
  console.log(cdsStr(c1));
  console.log(cdsStr(c2));
  c1 = {"i": 0, "s": "nil", "b": [122, 101, 114, 111], "m": {[1]: false}};
  console.log(cdsStr(c1));
  console.log(cdsStr(c2));
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

