// Generated by Mochi v0.10.42 on 2025-07-28 01:01:08 GMT+7

let PI: number = 3.141592653589793;
export interface Complex { re: number; im: number }
function sinApprox(x: number): number {
  let term: number = x;
  let sum: number = x;
  let n: number = 1;
  while ((n <= 10)) {
    let denom = ((2 * n) * ((2 * n) + 1));
    term = (((-term * x) * x) / denom);
    sum = (sum + term);
    n = (n + 1);
  }
  return sum;
}
function cosApprox(x: number): number {
  let term: number = 1.0;
  let sum: number = 1.0;
  let n: number = 1;
  while ((n <= 10)) {
    let denom = (((2 * n) - 1) * (2 * n));
    term = (((-term * x) * x) / denom);
    sum = (sum + term);
    n = (n + 1);
  }
  return sum;
}
function cis(x: number): Complex {
  return {"re": cosApprox(x), "im": sinApprox(x)};
}
function add(a: Complex, b: Complex): Complex {
  return {"re": (a.re + b.re), "im": (a.im + b.im)};
}
function sub(a: Complex, b: Complex): Complex {
  return {"re": (a.re - b.re), "im": (a.im - b.im)};
}
function mul(a: Complex, b: Complex): Complex {
  return {"re": ((a.re * b.re) - (a.im * b.im)), "im": ((a.re * b.im) + (a.im * b.re))};
}
function ditfft2Rec(x: number[], y: Complex[], offX: number, offY: number, n: number, s: number) {
  if ((n == 1)) {
    y[offY] = {"re": x[offX], "im": 0.0};
    return;
  }
  ditfft2Rec(x, y, offX, offY, Math.trunc(n / 2), (2 * s));
  ditfft2Rec(x, y, (offX + s), (offY + Math.trunc(n / 2)), Math.trunc(n / 2), (2 * s));
  let k: number = 0;
  while ((k < Math.trunc(n / 2))) {
    let angle: number = (((-2.0 * PI) * k) / n);
    let tf: Complex = mul(cis(angle), y[Math.trunc(((offY + k) + Math.trunc(n / 2)))]);
    let a: Complex = add(y[Math.trunc((offY + k))], tf);
    let b: Complex = sub(y[Math.trunc((offY + k))], tf);
    y[(offY + k)] = a;
    y[((offY + k) + Math.trunc(n / 2))] = b;
    k = (k + 1);
  }
}
function ditfft2(x: number[], y: Complex[], n: number, s: number) {
  ditfft2Rec(x, y, 0, 0, n, s);
}
function main() {
  let x: number[] = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0];
  let y: Complex[] = [];
  let i: number = 0;
  while ((i < (Array.isArray(x) || typeof x === 'string' ? x.length : Object.keys(x ?? {}).length))) {
    y.push({"re": 0.0, "im": 0.0});
    i = (i + 1);
  }
  ditfft2(x, y, (Array.isArray(x) || typeof x === 'string' ? x.length : Object.keys(x ?? {}).length), 1);
  for (const c of y) {
    let line: string = pad(fmt(c.re), 8);
    if ((c.im >= 0)) {
      line = ((line + "+") + fmt(c.im));
    } else {
      line = (line + fmt(c.im));
    }
    console.log(_str(line));
  }
}
function pad(s: string, w: number): string {
  let t: string = s;
  while (((Array.isArray(t) || typeof t === 'string' ? t.length : Object.keys(t ?? {}).length) < w)) {
    t = (" " + t);
  }
  return t;
}
function fmt(x: number): string {
  let y: number = (floorf(((x * 10000.0) + 0.5)) / 10000.0);
  let s: string = _str(y);
  let dot: number = s.indexOf(".");
  if ((dot == (0 - 1))) {
    s = (s + ".0000");
  } else {
    let d: bigint = (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) - dot) - 1);
    while ((d < 4)) {
      s = (s + "0");
      d = (d + 1);
    }
  }
  return s;
}
function floorf(x: number): number {
  let y: number = Math.trunc(x);
  return y;
}
function indexOf(s: string, ch: string): number {
  let i: number = 0;
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    if (((s).substring(i, (i + 1)) == ch)) {
      return i;
    }
    i = (i + 1);
  }
  return (0 - 1);
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

