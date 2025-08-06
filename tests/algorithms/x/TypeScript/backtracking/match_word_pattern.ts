function get_value(keys: string[], values: string[], key: string): string {
  let i: number = 0;
  while ((i < Number(Array.isArray(keys) || typeof keys === 'string' ? keys.length : Object.keys(keys ?? {}).length))) {
    if ((keys[i] == key)) {
      return values[i];
    }
    i = (i + 1);
  }
  return null;
}
function contains_value(values: string[], value: string): boolean {
  let i: number = 0;
  while ((i < Number(Array.isArray(values) || typeof values === 'string' ? values.length : Object.keys(values ?? {}).length))) {
    if ((values[i] == value)) {
      return true;
    }
    i = (i + 1);
  }
  return false;
}
function backtrack(pattern: string, input_string: string, pi: number, si: number, keys: string[], values: string[]): boolean {
  if (((pi == Number(Array.isArray(pattern) || typeof pattern === 'string' ? pattern.length : Object.keys(pattern ?? {}).length)) && (si == Number(Array.isArray(input_string) || typeof input_string === 'string' ? input_string.length : Object.keys(input_string ?? {}).length)))) {
    return true;
  }
  if (((pi == Number(Array.isArray(pattern) || typeof pattern === 'string' ? pattern.length : Object.keys(pattern ?? {}).length)) || (si == Number(Array.isArray(input_string) || typeof input_string === 'string' ? input_string.length : Object.keys(input_string ?? {}).length)))) {
    return false;
  }
  let ch: string = (pattern).substring(pi, (pi + 1));
  let mapped = get_value(keys, values, ch);
  if ((mapped != null)) {
    if (((input_string).substring(si, (si + Number(Array.isArray(mapped) || typeof mapped === 'string' ? mapped.length : Object.keys(mapped ?? {}).length))) == mapped)) {
      return backtrack(pattern, input_string, (pi + 1), (si + Number(Array.isArray(mapped) || typeof mapped === 'string' ? mapped.length : Object.keys(mapped ?? {}).length)), keys, values);
    }
    return false;
  }
  let end: number = (si + 1);
  while ((end <= Number(Array.isArray(input_string) || typeof input_string === 'string' ? input_string.length : Object.keys(input_string ?? {}).length))) {
    let substr = (input_string).substring(si, end);
    if (contains_value(values, substr)) {
      end = (end + 1);
      continue
    }
    let new_keys = [...keys, ch];
    let new_values = [...values, substr];
    if (backtrack(pattern, input_string, (pi + 1), end, new_keys, new_values)) {
      return true;
    }
    end = (end + 1);
  }
  return false;
}
function match_word_pattern(pattern: string, input_string: string): boolean {
  let keys: string[] = [];
  let values: string[] = [];
  return backtrack(pattern, input_string, 0, 0, keys, values);
}
function main() {
  console.log(_str(match_word_pattern("aba", "GraphTreesGraph")));
  console.log(_str(match_word_pattern("xyx", "PythonRubyPython")));
  console.log(_str(match_word_pattern("GG", "PythonJavaPython")));
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
  globalThis.gc?.()
  const _startMem = _mem()
  const _start = _now()
  main();
  const _end = _now()
  const _duration = _end - _start
  const _duration_us = Math.trunc(_duration / 1000)
  globalThis.gc?.()
  const _endMem = _mem()
  const _memory_bytes = Math.max(0, _endMem - _startMem)
  console.log(JSON.stringify({
    "duration_us": _duration_us,
    "memory_bytes": _memory_bytes,
    "name": "main"
  }, null, "  "))
})();

