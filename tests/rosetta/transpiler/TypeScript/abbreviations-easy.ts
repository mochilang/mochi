// Generated by Mochi v0.10.42 on 2025-07-28 10:39:57 GMT+7

function fields(s: string): string[] {
  let words: string[] = [];
  let cur: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    let ch: string = (s).substring(i, (i + 1));
    if ((((ch == " ") || (ch == "\n")) || (ch == "\t"))) {
      if (((Array.isArray(cur) || typeof cur === 'string' ? cur.length : Object.keys(cur ?? {}).length) > 0)) {
        words.push(cur);
        cur = "";
      }
    } else {
      cur = (cur + ch);
    }
    i = (i + 1);
  }
  if (((Array.isArray(cur) || typeof cur === 'string' ? cur.length : Object.keys(cur ?? {}).length) > 0)) {
    words.push(cur);
  }
  return words;
}
function padRight(s: string, width: number): string {
  let out: string = s;
  let i: number = (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length);
  while ((i < width)) {
    out = (out + " ");
    i = (i + 1);
  }
  return out;
}
function join(xs: string[], sep: string): string {
  let res: string = "";
  let i: number = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    if ((i > 0)) {
      res = (res + sep);
    }
    res = (res + xs[i]);
    i = (i + 1);
  }
  return res;
}
function validate(commands: string[], words: string[], mins: number[]): string[] {
  let results: string[] = [];
  if (((Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length) == 0)) {
    return results;
  }
  let wi: number = 0;
  while ((wi < (Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length))) {
    let w: string = words[wi];
    let found: boolean = false;
    let wlen: number = (Array.isArray(w) || typeof w === 'string' ? w.length : Object.keys(w ?? {}).length);
    let ci: number = 0;
    while ((ci < (Array.isArray(commands) || typeof commands === 'string' ? commands.length : Object.keys(commands ?? {}).length))) {
      let cmd: string = commands[ci];
      if ((((mins[ci] != 0) && (wlen >= mins[ci])) && (wlen <= (Array.isArray(cmd) || typeof cmd === 'string' ? cmd.length : Object.keys(cmd ?? {}).length)))) {
        let c: string = cmd.toUpperCase();
        let ww: string = w.toUpperCase();
        if (((c).substring(0, wlen) == ww)) {
          results.push(c);
          found = true;
          break
        }
      }
      ci = (ci + 1);
    }
    if (!found) {
      results.push("*error*");
    }
    wi = (wi + 1);
  }
  return results;
}
function main() {
  let table: string = (((((("Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find ") + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput ") + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO ") + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT ") + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT ") + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ");
  let commands: string[] = fields(table);
  let mins: number[] = [];
  let i: number = 0;
  while ((i < (Array.isArray(commands) || typeof commands === 'string' ? commands.length : Object.keys(commands ?? {}).length))) {
    let count: number = 0;
    let j: number = 0;
    let cmd: string = commands[i];
    while ((j < (Array.isArray(cmd) || typeof cmd === 'string' ? cmd.length : Object.keys(cmd ?? {}).length))) {
      let ch: string = (cmd).substring(j, (j + 1));
      if (((ch >= "A") && (ch <= "Z"))) {
        count = (count + 1);
      }
      j = (j + 1);
    }
    mins.push(count);
    i = (i + 1);
  }
  let sentence: string = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin";
  let words: string[] = fields(sentence);
  let results: string[] = validate(commands, words, mins);
  let out1: string = "user words:  ";
  let k: number = 0;
  while ((k < (Array.isArray(words) || typeof words === 'string' ? words.length : Object.keys(words ?? {}).length))) {
    out1 = ((out1 + padRight(words[k], (Array.isArray(results[k]) || typeof results[k] === 'string' ? results[k].length : Object.keys(results[k] ?? {}).length))) + " ");
    k = (k + 1);
  }
  console.log(_str(out1));
  console.log(_str(("full words:  " + join(results, " "))));
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

