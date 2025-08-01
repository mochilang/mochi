// Generated by Mochi v0.10.42 on 2025-07-28 10:40:01 GMT+7

function divisors(n: number): number[] {
  let divs: number[] = [1];
  let divs2: number[] = [];
  let i: number = 2;
  while (((i * i) <= n)) {
    if (((n % i) == 0)) {
      let j: number = Math.trunc(Math.trunc(n / i));
      divs.push(i);
      if ((i != j)) {
        divs2.push(j);
      }
    }
    i = (i + 1);
  }
  let j: number = ((Array.isArray(divs2) || typeof divs2 === 'string' ? divs2.length : Object.keys(divs2 ?? {}).length) - 1);
  while ((j >= 0)) {
    divs.push(divs2[j]);
    j = (j - 1);
  }
  return divs;
}
function sum(xs: number[]): number {
  let tot: number = 0;
  for (const v of xs) {
    tot = (tot + v);
  }
  return tot;
}
function sumStr(xs: number[]): string {
  let s: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    s = ((s + _str(xs[i])) + " + ");
    i = (i + 1);
  }
  return (s).substring(0, ((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) - 3));
}
function pad2(n: number): string {
  let s: string = _str(n);
  if (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) < 2)) {
    return (" " + s);
  }
  return s;
}
function pad5(n: number): string {
  let s: string = _str(n);
  while (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) < 5)) {
    s = (" " + s);
  }
  return s;
}
function abundantOdd(searchFrom: number, countFrom: number, countTo: number, printOne: boolean): number {
  let count: number = countFrom;
  let n: number = searchFrom;
  while ((count < countTo)) {
    let divs: number[] = divisors(n);
    let tot: number = divs.reduce((a, b) => a + b, 0.0);
    if ((tot > n)) {
      count = (count + 1);
      if ((printOne && (count < countTo))) {
        n = (n + 2);
        continue
      }
      let s: string = sumStr(divs);
      if (!printOne) {
        console.log(_str(((((((pad2(count) + ". ") + pad5(n)) + " < ") + s) + " = ") + _str(tot))));
      } else {
        console.log(_str(((((_str(n) + " < ") + s) + " = ") + _str(tot))));
      }
    }
    n = (n + 2);
  }
  return n;
}
function main() {
  let max = 25;
  console.log(_str((("The first " + _str(max)) + " abundant odd numbers are:")));
  let n = abundantOdd(1, 0, max, false);
  console.log(_str("\nThe one thousandth abundant odd number is:"));
  abundantOdd(n, max, 1000, true);
  console.log(_str("\nThe first abundant odd number above one billion is:"));
  abundantOdd(1000000001, 0, 1, true);
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

