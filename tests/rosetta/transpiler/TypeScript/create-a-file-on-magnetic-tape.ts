// Generated by Mochi v0.10.55 on 2025-08-02 20:23:39 GMT+7

export interface Hdr { Name: string; Mode: number; Size: number; ModTime: number; Typeflag: number; Uname: string; Gname: string }
function gzipWriter(w) {
  return w;
}
function tarWriter(w) {
  return w;
}
function tarWriteHeader(w, hdr: Record<string, any>) {
}
function tarWrite(w, data: string) {
}
function main() {
  let filename: string = "TAPE.FILE";
  let data: string = "";
  let outfile: string = "";
  let gzipFlag: boolean = false;
  let w = null;
  if ((outfile != "")) {
    w = null;
  }
  if (gzipFlag) {
    w = gzipWriter(w);
  }
  w = tarWriter(w);
  let hdr: Hdr = {"Name": filename, "Mode": 432, "Size": Number(Array.isArray(data) || typeof data === 'string' ? data.length : Object.keys(data ?? {}).length), "ModTime": _now(), "Typeflag": 0, "Uname": "guest", "Gname": "guest"};
  tarWriteHeader(w, hdr);
  tarWrite(w, data);
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
    return (Deno.memoryUsage?.().heapUsed ?? 0);
  }
  if (typeof process !== 'undefined') {
    return process.memoryUsage().heapUsed;
  }
  return 0;
}
(() => {
  globalThis.gc?.()
  const _startMem = _mem()
  const _start = _now()
  main();
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  const _endMem = _mem()
  globalThis.gc?.()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

