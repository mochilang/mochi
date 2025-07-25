// Generated by Mochi v0.10.40 on 2025-07-25 17:22:23 GMT+7

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
{
  const _startMem = _mem()
  const _start = _now()
  function sortRunes(s: string): string {
    let arr: string[] = [];
    let i: number = 0;
    while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
      arr = [...arr, s.slice(i, (i + 1))];
      i = (i + 1);
    }
    let n: number = (Array.isArray(arr) || typeof arr === 'string' ? arr.length : Object.keys(arr ?? {}).length);
    let m: number = 0;
    while ((m < n)) {
      let j: number = 0;
      while ((j < (n - 1))) {
        if ((arr[j] > arr[Math.trunc((j + 1))])) {
          const tmp: string = arr[j];
          arr[j] = arr[Math.trunc((j + 1))];
          arr[(j + 1)] = tmp;
        }
        j = (j + 1);
      }
      m = (m + 1);
    }
    let out: string = "";
    i = 0;
    while ((i < n)) {
      out = (out + arr[i]);
      i = (i + 1);
    }
    return out;
  }
  function deranged(a: string, b: string): boolean {
    if (((Array.isArray(a) || typeof a === 'string' ? a.length : Object.keys(a ?? {}).length) != (Array.isArray(b) || typeof b === 'string' ? b.length : Object.keys(b ?? {}).length))) {
      return false;
    }
    let i: number = 0;
    while ((i < (Array.isArray(a) || typeof a === 'string' ? a.length : Object.keys(a ?? {}).length))) {
      if ((a.slice(i, (i + 1)) == b.slice(i, (i + 1)))) {
        return false;
      }
      i = (i + 1);
    }
    return true;
  }
  function main() {
    const words: string[] = ["constitutionalism", "misconstitutional"];
    let m: Record<string, string[]> = {};
    let bestLen: number = 0;
    let w1: string = "";
    let w2: string = "";
    for (const w of words) {
      if (((Array.isArray(w) || typeof w === 'string' ? w.length : Object.keys(w ?? {}).length) <= bestLen)) {
        continue
      }
      const k = sortRunes(w);
      if (!(k in m)) {
        m[k] = [w];
        continue
      }
      for (const c of m[k]) {
        if (deranged(w, c)) {
          bestLen = (Array.isArray(w) || typeof w === 'string' ? w.length : Object.keys(w ?? {}).length);
          w1 = c;
          w2 = w;
          break
        }
      }
      m[k] = [...m[k], w];
    }
    console.log(((((w1 + " ") + w2) + " : Length ") + String(bestLen)));
  }
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
}

