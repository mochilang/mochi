// Generated by Mochi v0.10.41 on 2025-07-27 11:47:01 GMT+7

function xor(a: number, b: number): number {
  let res: number = 0;
  let bit: number = 1;
  let x: number = a;
  let y: number = b;
  while (((x > 0) || (y > 0))) {
    if (((((x % 2) + (y % 2)) % 2) == 1)) {
      res = (res + bit);
    }
    x = Math.trunc(x / 2);
    y = Math.trunc(y / 2);
    bit = (bit * 2);
  }
  return res;
}
function enc(b: number): number {
  return xor(b, Math.trunc(b / 2));
}
function dec(g: number): number {
  let b: number = 0;
  let x: number = g;
  while ((x > 0)) {
    b = xor(b, x);
    x = Math.trunc(x / 2);
  }
  return b;
}
function binary(n: number): string {
  if ((n == 0)) {
    return "0";
  }
  let s: string = "";
  let x: number = n;
  while ((x > 0)) {
    if (((x % 2) == 1)) {
      s = ("1" + s);
    } else {
      s = ("0" + s);
    }
    x = Math.trunc(x / 2);
  }
  return s;
}
function pad5(s: string): string {
  let p: string = s;
  while (((Array.isArray(p) || typeof p === 'string' ? p.length : Object.keys(p ?? {}).length) < 5)) {
    p = ("0" + p);
  }
  return p;
}
function main() {
  console.log("decimal  binary   gray    decoded");
  let b: number = 0;
  while ((b < 32)) {
    let g: number = enc(b);
    let d: number = dec(g);
    console.log(((((((("  " + pad5(binary(b))) + "   ") + pad5(binary(g))) + "   ") + pad5(binary(d))) + "  ") + String(d)));
    b = (b + 1);
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

