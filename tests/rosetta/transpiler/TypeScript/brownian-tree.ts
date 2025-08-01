// Generated by Mochi v0.10.42 on 2025-07-27 20:13:16 GMT+7

let w: number = 400;
let h: number = 300;
let n: number = 15000;
let frost: number = 255;
let grid: number[][] = [];
let y: number = 0;
function inBounds(x: number, y: number): boolean {
  return ((((x >= 0) && (x < w)) && (y >= 0)) && (y < h));
}
function hasNeighbor(x: number, y: number): boolean {
  let dy: number = -1;
  while ((dy <= 1)) {
    let dx: number = -1;
    while ((dx <= 1)) {
      if (!((dx == 0) && (dy == 0))) {
        let nx: number = (x + dx);
        let ny: number = (y + dy);
        if ((inBounds(nx, ny) && (grid[ny][nx] == frost))) {
          return true;
        }
      }
      dx = (dx + 1);
    }
    dy = (dy + 1);
  }
  return false;
}
let a: number = 0;
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
  while ((y < h)) {
    let row: number[] = [];
    let x: number = 0;
    while ((x < w)) {
      row.push(0);
      x = (x + 1);
    }
    grid.push(row);
    y = (y + 1);
  }
  grid[Math.trunc(h / 3)][Math.trunc(w / 3)] = frost;
  while ((a < n)) {
    let px: number = (_now() % w);
    let py: number = (_now() % h);
    if ((grid[py][px] == frost)) {
      let lost: boolean = false;
      while (true) {
        px = ((px + (_now() % 3)) - 1);
        py = ((py + (_now() % 3)) - 1);
        if (!inBounds(px, py)) {
          lost = true;
          break
        }
        if ((grid[py][px] != frost)) {
          break
        }
      }
      if (lost) {
        continue
      }
    } else {
      let lost: boolean = false;
      while (!hasNeighbor(px, py)) {
        px = ((px + (_now() % 3)) - 1);
        py = ((py + (_now() % 3)) - 1);
        if (!inBounds(px, py)) {
          lost = true;
          break
        }
      }
      if (lost) {
        continue
      }
    }
    grid[py][px] = frost;
    a = (a + 1);
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

