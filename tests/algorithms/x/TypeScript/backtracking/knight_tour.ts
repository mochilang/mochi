function get_valid_pos(position: number[], n: number): number[][] {
  let y: number = position[Math.trunc(0)];
  let x: number = position[Math.trunc(1)];
  let positions: number[][] = [[(y + 1), (x + 2)], [(y - 1), (x + 2)], [(y + 1), (x - 2)], [(y - 1), (x - 2)], [(y + 2), (x + 1)], [(y + 2), (x - 1)], [(y - 2), (x + 1)], [(y - 2), (x - 1)]];
  let permissible: number[][] = [];
  for (let idx = 0; idx < Number(Array.isArray(positions) || typeof positions === 'string' ? positions.length : Object.keys(positions ?? {}).length); idx++) {
    let inner: number[] = positions[idx];
    let y_test: number = inner[Math.trunc(0)];
    let x_test: number = inner[Math.trunc(1)];
    if (((((y_test >= 0) && (y_test < n)) && (x_test >= 0)) && (x_test < n))) {
      permissible.push(inner);
    }
  }
  return permissible;
}
function is_complete(board: number[][]): boolean {
  for (let i = 0; i < Number(Array.isArray(board) || typeof board === 'string' ? board.length : Object.keys(board ?? {}).length); i++) {
    let row: number[] = board[i];
    for (let j = 0; j < Number(Array.isArray(row) || typeof row === 'string' ? row.length : Object.keys(row ?? {}).length); j++) {
      if ((row[j] == 0)) {
        return false;
      }
    }
  }
  return true;
}
function open_knight_tour_helper(board: number[][], pos: number[], curr: number): boolean {
  if (is_complete(board)) {
    return true;
  }
  let moves: number[][] = get_valid_pos(pos, Number(Array.isArray(board) || typeof board === 'string' ? board.length : Object.keys(board ?? {}).length));
  for (let i = 0; i < Number(Array.isArray(moves) || typeof moves === 'string' ? moves.length : Object.keys(moves ?? {}).length); i++) {
    let position: number[] = moves[i];
    let y: number = position[Math.trunc(0)];
    let x: number = position[Math.trunc(1)];
    if ((board[y][x] == 0)) {
      board[y][x] = (curr + 1);
      if (open_knight_tour_helper(board, position, (curr + 1))) {
        return true;
      }
      board[y][x] = 0;
    }
  }
  return false;
}
function open_knight_tour(n: number): number[][] {
  let board: number[][] = [];
  for (let i = 0; i < n; i++) {
    let row: number[] = [];
    for (let j = 0; j < n; j++) {
      row.push(0);
    }
    board.push(row);
  }
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      board[i][j] = 1;
      if (open_knight_tour_helper(board, [i, j], 1)) {
        return board;
      }
      board[i][j] = 0;
    }
  }
  console.log(_str(("Open Knight Tour cannot be performed on a board of size " + _str(n))));
  return board;
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
let board: number[][]
(() => {
  globalThis.gc?.()
  const _startMem = _mem()
  const _start = _now()
board = open_knight_tour(1)
  console.log(_str(board[Math.trunc(0)][Math.trunc(0)]));
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

