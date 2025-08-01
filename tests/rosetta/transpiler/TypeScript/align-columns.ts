// Generated by Mochi v0.10.42 on 2025-07-28 10:40:08 GMT+7

function split(s: string, sep: string): string[] {
  let parts: string[] = [];
  let cur: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    if (((((Array.isArray(sep) || typeof sep === 'string' ? sep.length : Object.keys(sep ?? {}).length) > 0) && ((i + (Array.isArray(sep) || typeof sep === 'string' ? sep.length : Object.keys(sep ?? {}).length)) <= (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) && ((s).substring(i, (i + (Array.isArray(sep) || typeof sep === 'string' ? sep.length : Object.keys(sep ?? {}).length))) == sep))) {
      parts.push(cur);
      cur = "";
      i = (i + (Array.isArray(sep) || typeof sep === 'string' ? sep.length : Object.keys(sep ?? {}).length));
    } else {
      cur = (cur + (s).substring(i, (i + 1)));
      i = (i + 1);
    }
  }
  parts.push(cur);
  return parts;
}
function rstripEmpty(words: string[]): string[] {
  let n: number = (Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length);
  while (((n > 0) && (words[Math.trunc((n - 1))] == ""))) {
    n = (n - 1);
  }
  return words.slice(0, n);
}
function spaces(n: number): string {
  let out: string = "";
  let i: number = 0;
  while ((i < n)) {
    out = (out + " ");
    i = (i + 1);
  }
  return out;
}
function pad(word: string, width: number, align: number): string {
  let diff: number = (width - (Array.isArray(word) || typeof word === 'string' ? word.length : Object.keys(word ?? {}).length));
  if ((align == 0)) {
    return (word + spaces(diff));
  }
  if ((align == 2)) {
    return (spaces(diff) + word);
  }
  let left: number = Math.trunc(Math.trunc(diff / 2));
  let right: number = (diff - left);
  return ((spaces(left) + word) + spaces(right));
}
function newFormatter(text: string): Record<string, any> {
  let lines: string[] = text.split("\n");
  let fmtLines: string[][] = [];
  let width: number[] = [];
  let i: number = 0;
  while ((i < (Array.isArray(lines) || typeof lines === 'string' ? lines.length : Object.keys(lines ?? {}).length))) {
    if (((Array.isArray(lines[i]) || typeof lines[i] === 'string' ? lines[i].length : Object.keys(lines[i] ?? {}).length) == 0)) {
      i = (i + 1);
      continue
    }
    let words: string[] = rstripEmpty(lines[i].split("$"));
    fmtLines.push(words);
    let j: number = 0;
    while ((j < (Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length))) {
      let wlen: number = (Array.isArray(words[j]) || typeof words[j] === 'string' ? words[j].length : Object.keys(words[j] ?? {}).length);
      if ((j == (Array.isArray(width) || typeof width === 'string' ? width.length : Object.keys(width ?? {}).length))) {
        width.push(wlen);
      } else {
        if ((wlen > width[j])) {
          width[j] = wlen;
        }
      }
      j = (j + 1);
    }
    i = (i + 1);
  }
  return {"text": fmtLines, width};
}
function printFmt(f: Record<string, any>, align: number) {
  let lines: string[][] = f.text;
  let width: number[] = f.width;
  let i: number = 0;
  while ((i < (Array.isArray(lines) || typeof lines === 'string' ? lines.length : Object.keys(lines ?? {}).length))) {
    let words: string[] = lines[i];
    let line: string = "";
    let j: number = 0;
    while ((j < (Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length))) {
      line = ((line + pad(words[j], width[j], align)) + " ");
      j = (j + 1);
    }
    console.log(_str(line));
    i = (i + 1);
  }
  console.log(_str(""));
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
  let text: string = ((((("Given$a$text$file$of$many$lines,$where$fields$within$a$line\n" + "are$delineated$by$a$single$'dollar'$character,$write$a$program\n") + "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each\n") + "column$are$separated$by$at$least$one$space.\n") + "Further,$allow$for$each$word$in$a$column$to$be$either$left\n") + "justified,$right$justified,$or$center$justified$within$its$column.");
  let f: Record<string, any> = newFormatter(text);
  printFmt(f, 0);
  printFmt(f, 1);
  printFmt(f, 2);
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

