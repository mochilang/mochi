// Generated by Mochi v0.10.42 on 2025-07-27 21:21:18 GMT+7

function indexOf(s: string, ch: string): number {
  let i: number = 0;
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    if (((s).substring(i, (i + 1)) == ch)) {
      return i;
    }
    i = (i + 1);
  }
  return -1;
}
function fmt1(x: number): string {
  let y: number = (Math.trunc(((x * 10.0) + 0.5)) / 10.0);
  let s: string = String(y);
  let dot: number = s.indexOf(".");
  if ((dot < 0)) {
    s = (s + ".0");
  }
  return s;
}
function listToString1(xs: number[]): string {
  let s: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    s = (s + fmt1(xs[i]));
    if ((i < ((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
      s = (s + " ");
    }
    i = (i + 1);
  }
  return (s + "]");
}
function deconv(g: number[], f: number[]): number[] {
  let out: number[] = [];
  let i: number = 0;
  while ((i <= ((Array.isArray(g) || typeof g === 'string' ? g.length : Object.keys(g ?? {}).length) - (Array.isArray(f) || typeof f === 'string' ? f.length : Object.keys(f ?? {}).length)))) {
    let sum: number = g[i];
    let j: number = 1;
    while ((j < (Array.isArray(f) || typeof f === 'string' ? f.length : Object.keys(f ?? {}).length))) {
      if ((j <= i)) {
        sum = (sum - (out[Math.trunc((i - j))] * f[j]));
      }
      j = (j + 1);
    }
    out.push((sum / f[Math.trunc(0)]));
    i = (i + 1);
  }
  return out;
}
function main() {
  let h: number[] = [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0];
  let f: number[] = [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0];
  let g: number[] = [24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0];
  console.log(listToString1(h));
  console.log(listToString1(deconv(g, f)));
  console.log(listToString1(f));
  console.log(listToString1(deconv(g, h)));
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

