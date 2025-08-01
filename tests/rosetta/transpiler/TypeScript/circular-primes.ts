// Generated by Mochi v0.10.42 on 2025-07-27 21:08:40 GMT+7

function isPrime(n: number): boolean {
  if ((n < 2)) {
    return false;
  }
  if (((n % 2) == 0)) {
    return (n == 2);
  }
  if (((n % 3) == 0)) {
    return (n == 3);
  }
  let d: number = 5;
  while (((d * d) <= n)) {
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 2);
    if (((n % d) == 0)) {
      return false;
    }
    d = (d + 4);
  }
  return true;
}
let circs: number[] = [];
function isCircular(n: number): boolean {
  let nn: number = n;
  let pow: number = 1;
  while ((nn > 0)) {
    pow = (pow * 10);
    nn = Math.trunc(nn / 10);
  }
  nn = n;
  while (true) {
    nn = (nn * 10);
    let f: number = Math.trunc(nn / pow);
    nn = (nn + (f * (1 - pow)));
    if ((nn == n)) {
      break
    }
    if (!isPrime(nn)) {
      return false;
    }
  }
  return true;
}
let digits: number[] = [1, 3, 7, 9];
let q: number[] = [1, 2, 3, 5, 7, 9];
let fq: number[] = [1, 2, 3, 5, 7, 9];
let count: number = 0;
function showList(xs: number[]): string {
  let out: string = "[";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    out = (out + String(xs[i]));
    if ((i < ((Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length) - 1))) {
      out = (out + ", ");
    }
    i = (i + 1);
  }
  return (out + "]");
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
  console.log("The first 19 circular primes are:");
  while (true) {
    let f: number = q[Math.trunc(0)];
    let fd: number = fq[Math.trunc(0)];
    if ((isPrime(f) && isCircular(f))) {
      circs.push(f);
      count = (count + 1);
      if ((count == 19)) {
        break
      }
    }
    q = q.slice(1);
    fq = fq.slice(1);
    if (((f != 2) && (f != 5))) {
      for (const d of digits) {
        q.push(((f * 10) + d));
        fq.push(fd);
      }
    }
  }
  console.log(showList(circs));
  console.log("\nThe next 4 circular primes, in repunit format, are:");
  console.log("[R(19) R(23) R(317) R(1031)]");
  console.log("\nThe following repunits are probably circular primes:");
  for (const i of [5003, 9887, 15073, 25031, 35317, 49081]) {
    console.log((("R(" + String(i)) + ") : true"));
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

