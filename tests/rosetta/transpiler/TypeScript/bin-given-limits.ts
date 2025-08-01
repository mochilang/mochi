// Generated by Mochi v0.10.42 on 2025-07-27 20:11:31 GMT+7

function getBins(limits: number[], data: number[]): number[] {
  let n: number = (Array.isArray(limits) || typeof limits === 'string' ? limits.length : Object.keys(limits ?? {}).length);
  let bins: number[] = [];
  let i: number = 0;
  while ((i < (n + 1))) {
    bins.push(0);
    i = (i + 1);
  }
  let j: number = 0;
  while ((j < (Array.isArray(data) || typeof data === 'string' ? data.length : Object.keys(data ?? {}).length))) {
    let d: number = data[j];
    let index: number = 0;
    while ((index < (Array.isArray(limits) || typeof limits === 'string' ? limits.length : Object.keys(limits ?? {}).length))) {
      if ((d < limits[index])) {
        break
      }
      if ((d == limits[index])) {
        index = (index + 1);
        break
      }
      index = (index + 1);
    }
    bins[index] = (bins[index] + 1);
    j = (j + 1);
  }
  return bins;
}
function padLeft(n: number, width: number): string {
  let s: string = String(n);
  let pad: number = (width - (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length));
  let out: string = "";
  let i: number = 0;
  while ((i < pad)) {
    out = (out + " ");
    i = (i + 1);
  }
  return (out + s);
}
function printBins(limits: number[], bins: number[]) {
  let n: number = (Array.isArray(limits) || typeof limits === 'string' ? limits.length : Object.keys(limits ?? {}).length);
  console.log(((("           < " + padLeft(limits[Math.trunc(0)], 3)) + " = ") + padLeft(bins[Math.trunc(0)], 2)));
  let i: number = 1;
  while ((i < n)) {
    console.log((((((">= " + padLeft(limits[Math.trunc((i - 1))], 3)) + " and < ") + padLeft(limits[i], 3)) + " = ") + padLeft(bins[i], 2)));
    i = (i + 1);
  }
  console.log((((">= " + padLeft(limits[Math.trunc((n - 1))], 3)) + "           = ") + padLeft(bins[n], 2)));
  console.log("");
}
function main() {
  let limitsList: number[][] = [[23, 37, 43, 53, 67, 83], [14, 18, 249, 312, 389, 392, 513, 591, 634, 720]];
  let dataList: number[][] = [[95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55], [445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749]];
  let i: number = 0;
  while ((i < (Array.isArray(limitsList) || typeof limitsList === 'string' ? limitsList.length : Object.keys(limitsList ?? {}).length))) {
    console.log((("Example " + String((i + 1))) + "\n"));
    let bins: number[] = getBins(limitsList[i], dataList[i]);
    printBins(limitsList[i], bins);
    i = (i + 1);
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

