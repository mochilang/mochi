// Generated by Mochi v0.10.40 on 2025-07-26 09:24:50 GMT+7

function strdup(s: string): string {
  return (s + "");
}
function main() {
  let go1: string = "hello C";
  let c2: string = strdup(go1);
  console.log(c2);
}
var _nowSeed = 1992085998;
var _nowSeeded = true;
function _now(): number {
  _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
  return _nowSeed;
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

