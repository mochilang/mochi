// Generated by Mochi v0.10.42 on 2025-07-27 21:44:51 GMT+7

function padRight(s: string, w: number): string {
  let r: string = s;
  while (((Array.isArray(r) || typeof r === 'string' ? r.length : Object.keys(r ?? {}).length) < w)) {
    r = (r + " ");
  }
  return r;
}
function linearCombo(c: number[]): string {
  let out: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(c) || typeof c === 'string' ? c.length : Object.keys(c ?? {}).length))) {
    let n: number = c[i];
    if ((n != 0)) {
      let op: string = "";
      if (((n < 0) && ((Array.isArray(out) || typeof out === 'string' ? out.length : Object.keys(out ?? {}).length) == 0))) {
        op = "-";
      } else {
        if ((n < 0)) {
          op = " - ";
        } else {
          if (((n > 0) && ((Array.isArray(out) || typeof out === 'string' ? out.length : Object.keys(out ?? {}).length) == 0))) {
            op = "";
          } else {
            op = " + ";
          }
        }
      }
      let av: number = n;
      if ((av < 0)) {
        av = -av;
      }
      let coeff: string = (String(av) + "*");
      if ((av == 1)) {
        coeff = "";
      }
      out = (((((out + op) + coeff) + "e(") + String((i + 1))) + ")");
    }
    i = (i + 1);
  }
  if (((Array.isArray(out) || typeof out === 'string' ? out.length : Object.keys(out ?? {}).length) == 0)) {
    return "0";
  }
  return out;
}
function main() {
  let combos: number[][] = [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, -3], [-1]];
  let idx: number = 0;
  while ((idx < (Array.isArray(combos) || typeof combos === 'string' ? combos.length : Object.keys(combos ?? {}).length))) {
    let c: number[] = combos[idx];
    let t: string = "[";
    let j: number = 0;
    while ((j < (Array.isArray(c) || typeof c === 'string' ? c.length : Object.keys(c ?? {}).length))) {
      t = (t + String(c[j]));
      if ((j < ((Array.isArray(c) || typeof c === 'string' ? c.length : Object.keys(c ?? {}).length) - 1))) {
        t = (t + ", ");
      }
      j = (j + 1);
    }
    t = (t + "]");
    let lc: string = linearCombo(c);
    console.log(((padRight(t, 15) + "  ->  ") + lc));
    idx = (idx + 1);
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

