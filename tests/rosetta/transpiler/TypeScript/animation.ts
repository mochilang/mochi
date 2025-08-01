// Generated by Mochi v0.10.42 on 2025-07-27 18:44:58 GMT+7

let msg: string = "Hello World! ";
let shift: number = 0;
let inc: number = 1;
let clicks: number = 0;
let frames: number = 0;
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
  while ((clicks < 5)) {
    let line: string = "";
    let i: number = 0;
    while ((i < (Array.isArray(msg) || typeof msg === 'string' ? msg.length : Object.keys(msg ?? {}).length))) {
      let idx: number = ((shift + i) % (Array.isArray(msg) || typeof msg === 'string' ? msg.length : Object.keys(msg ?? {}).length));
      line = (line + msg.slice(idx, (idx + 1)));
      i = (i + 1);
    }
    console.log(line);
    shift = ((shift + inc) % (Array.isArray(msg) || typeof msg === 'string' ? msg.length : Object.keys(msg ?? {}).length));
    frames = (frames + 1);
    if (((frames % (Array.isArray(msg) || typeof msg === 'string' ? msg.length : Object.keys(msg ?? {}).length)) == 0)) {
      inc = ((Array.isArray(msg) || typeof msg === 'string' ? msg.length : Object.keys(msg ?? {}).length) - inc);
      clicks = (clicks + 1);
    }
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

