// Generated by Mochi v0.10.42 on 2025-07-27 20:34:08 GMT+7

let animal: string[] = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"];
let yinYang: string[] = ["Yang", "Yin"];
let element: string[] = ["Wood", "Fire", "Earth", "Metal", "Water"];
let stemChArr: string[] = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"];
let branchChArr: string[] = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"];
export interface Info { animal: string; yinYang: string; element: string; stemBranch: string; cycle: number }
function cz(yr: number, animal: string[], yinYang: string[], element: string[], sc: string[], bc: string[]): Info {
  let y: number = (yr - 4);
  let stem: number = (y % 10);
  let branch: number = (y % 12);
  let sb: string = (sc[stem] + bc[branch]);
  return {"animal": animal[branch], "yinYang": yinYang[Math.trunc((stem % 2))], "element": element[Math.trunc(Math.trunc(Math.trunc(stem / 2)))], "stemBranch": sb, "cycle": ((y % 60) + 1)};
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
  for (const yr of [1935, 1938, 1968, 1972, 1976]) {
    let r: Info = cz(yr, animal, yinYang, element, stemChArr, branchChArr);
    console.log(((((((((((String(yr) + ": ") + r.element) + " ") + r.animal) + ", ") + r.yinYang) + ", Cycle year ") + String(r.cycle)) + " ") + r.stemBranch));
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

