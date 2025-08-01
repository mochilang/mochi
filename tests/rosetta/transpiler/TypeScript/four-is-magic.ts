// Generated by Mochi v0.10.42 on 2025-07-28 10:19:07 GMT+7

function capitalize(s: string): string {
  if (((Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length) == 0)) {
    return s;
  }
  return ((s).substring(0, 1).toUpperCase() + (s).substring(1, (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length)));
}
let small: string[] = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
let tens: string[] = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
let illions: string[] = ["", " thousand", " million", " billion", " trillion", " quadrillion", " quintillion"];
function say(n: number): string {
  let t: string = "";
  if ((n < 0)) {
    t = "negative ";
    n = -n;
  }
  if ((n < 20)) {
    return (t + small[n]);
  } else {
    if ((n < 100)) {
      t = tens[Math.trunc(Math.trunc(n / 10))];
      let s: bigint = (n % 10);
      if ((s > 0)) {
        t = ((t + "-") + small[s]);
      }
      return t;
    } else {
      if ((n < 1000)) {
        t = (small[Math.trunc(Math.trunc(n / 100))] + " hundred");
        let s: bigint = (n % 100);
        if ((s > 0)) {
          t = ((t + " ") + say(s));
        }
        return t;
      }
    }
  }
  let sx: string = "";
  let i: number = 0;
  let nn: number = n;
  while ((nn > 0)) {
    let p: bigint = (nn % 1000);
    nn = Math.trunc(nn / 1000);
    if ((p > 0)) {
      let ix: string = (say(p) + illions[i]);
      if ((sx != "")) {
        ix = ((ix + " ") + sx);
      }
      sx = ix;
    }
    i = (i + 1);
  }
  return (t + sx);
}
function fourIsMagic(n: number): string {
  let s: string = say(n);
  s = capitalize(s);
  let t: string = s;
  while ((n != 4)) {
    n = (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length);
    s = say(n);
    t = ((((t + " is ") + s) + ", ") + s);
  }
  t = (t + " is magic.");
  return t;
}
function main() {
  let nums: number[] = [0, 4, 6, 11, 13, 75, 100, 337, -164, 9223372036854775807];
  for (const n of nums) {
    console.log(_str(fourIsMagic(n)));
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

