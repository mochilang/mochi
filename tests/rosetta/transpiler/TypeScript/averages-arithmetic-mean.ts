// Generated by Mochi v0.10.42 on 2025-07-27 18:45:44 GMT+7

function mean(v: number[]): Record<string, any> {
  if (((Array.isArray(v) || typeof v === 'string' ? v.length : Object.keys(v ?? {}).length) == 0)) {
    return {"ok": false};
  }
  let sum: number = 0.0;
  let i: number = 0;
  while ((i < (Array.isArray(v) || typeof v === 'string' ? v.length : Object.keys(v ?? {}).length))) {
    sum = (sum + v[i]);
    i = (i + 1);
  }
  return {"ok": true, "mean": (sum / (Array.isArray(v) || typeof v === 'string' ? v.length : Object.keys(v ?? {}).length))};
}
function main() {
  let sets: any[][] = [[], [3.0, 1.0, 4.0, 1.0, 5.0, 9.0], [100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, -100000000000000000000.0], [10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11], [10.0, 20.0, 30.0, 40.0, 50.0, -100.0, 4.7, -1100.0]];
  for (const v of sets) {
    console.log(("Vector: " + String(v)));
    let r: Record<string, any> = mean(v);
    if (r.ok) {
      console.log(((("Mean of " + String((Array.isArray(v) || typeof v === 'string' ? v.length : Object.keys(v ?? {}).length))) + " numbers is ") + String(r.mean)));
    } else {
      console.log("Mean undefined");
    }
    console.log("");
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

