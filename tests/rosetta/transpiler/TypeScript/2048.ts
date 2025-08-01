// Generated by Mochi v0.10.42 on 2025-07-28 10:39:51 GMT+7

let SIZE: number = 4;
export interface Board { cells: number[][] }
export interface SpawnResult { board: Board; full: boolean }
export interface SlideResult { row: number[]; gain: number }
export interface MoveResult { board: Board; score: number; moved: boolean }
function newBoard(): Board {
  let b: number[][] = [];
  let y: number = 0;
  while ((y < SIZE)) {
    let row: number[] = [];
    let x: number = 0;
    while ((x < SIZE)) {
      row.push(0);
      x = (x + 1);
    }
    b.push(row);
    y = (y + 1);
  }
  return {"cells": b};
}
function spawnTile(b: Board): SpawnResult {
  let grid: number[][] = b.cells;
  let empty: number[][] = [];
  let y: number = 0;
  while ((y < SIZE)) {
    let x: number = 0;
    while ((x < SIZE)) {
      if ((grid[y][x] == 0)) {
        empty.push([x, y]);
      }
      x = (x + 1);
    }
    y = (y + 1);
  }
  if (((Array.isArray(empty) || typeof empty === 'string' ? empty.length : Object.keys(empty ?? {}).length) == 0)) {
    return {"board": b, "full": true};
  }
  let idx: number = (_now() % (Array.isArray(empty) || typeof empty === 'string' ? empty.length : Object.keys(empty ?? {}).length));
  let cell: number[] = empty[idx];
  let val: number = 4;
  if (((_now() % 10) < 9)) {
    val = 2;
  }
  grid[cell[Math.trunc(1)]][cell[Math.trunc(0)]] = val;
  return {"board": {"cells": grid}, "full": ((Array.isArray(empty) || typeof empty === 'string' ? empty.length : Object.keys(empty ?? {}).length) == 1)};
}
function pad(n: number): string {
  let s: string = _str(n);
  let pad: number = (4 - (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length));
  let i: number = 0;
  let out: string = "";
  while ((i < pad)) {
    out = (out + " ");
    i = (i + 1);
  }
  return (out + s);
}
function draw(b: Board, score: number) {
  console.log(_str(("Score: " + _str(score))));
  let y: number = 0;
  while ((y < SIZE)) {
    console.log(_str("+----+----+----+----+"));
    let line: string = "|";
    let x: number = 0;
    while ((x < SIZE)) {
      let v: number = b.cells[y][x];
      if ((v == 0)) {
        line = (line + "    |");
      } else {
        line = ((line + pad(v)) + "|");
      }
      x = (x + 1);
    }
    console.log(_str(line));
    y = (y + 1);
  }
  console.log(_str("+----+----+----+----+"));
  console.log(_str("W=Up S=Down A=Left D=Right Q=Quit"));
}
function reverseRow(r: number[]): number[] {
  let out: number[] = [];
  let i: number = ((Array.isArray(r) || typeof r === 'string' ? r.length : Object.keys(r ?? {}).length) - 1);
  while ((i >= 0)) {
    out.push(r[i]);
    i = (i - 1);
  }
  return out;
}
function slideLeft(row: number[]): SlideResult {
  let xs: number[] = [];
  let i: number = 0;
  while ((i < (Array.isArray(row) || typeof row === 'string' ? row.length : Object.keys(row ?? {}).length))) {
    if ((row[i] != 0)) {
      xs.push(row[i]);
    }
    i = (i + 1);
  }
  let res: number[] = [];
  let gain: number = 0;
  i = 0;
  while ((i < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length))) {
    if ((((i + 1) < (Array.isArray(xs) || typeof xs === 'string' ? xs.length : Object.keys(xs ?? {}).length)) && (xs[i] == xs[Math.trunc((i + 1))]))) {
      let v: number = (xs[i] * 2);
      gain = (gain + v);
      res.push(v);
      i = (i + 2);
    } else {
      res.push(xs[i]);
      i = (i + 1);
    }
  }
  while (((Array.isArray(res) || typeof res === 'string' ? res.length : Object.keys(res ?? {}).length) < SIZE)) {
    res.push(0);
  }
  return {"row": res, gain};
}
function moveLeft(b: Board, score: number): MoveResult {
  let grid: number[][] = b.cells;
  let moved: boolean = false;
  let y: number = 0;
  while ((y < SIZE)) {
    let r: SpawnResult = slideLeft(grid[y]);
    let _new = r.row;
    score = (score + r.gain);
    let x: number = 0;
    while ((x < SIZE)) {
      if ((grid[y][x] != _new[x])) {
        moved = true;
      }
      grid[y][x] = _new[x];
      x = (x + 1);
    }
    y = (y + 1);
  }
  return {"board": {"cells": grid}, score, moved};
}
function moveRight(b: Board, score: number): MoveResult {
  let grid: number[][] = b.cells;
  let moved: boolean = false;
  let y: number = 0;
  while ((y < SIZE)) {
    let rev: number[] = reverseRow(grid[y]);
    let r: SpawnResult = slideLeft(rev);
    rev = r.row;
    score = (score + r.gain);
    rev = reverseRow(rev);
    let x: number = 0;
    while ((x < SIZE)) {
      if ((grid[y][x] != rev[x])) {
        moved = true;
      }
      grid[y][x] = rev[x];
      x = (x + 1);
    }
    y = (y + 1);
  }
  return {"board": {"cells": grid}, score, moved};
}
function getCol(b: Board, x: number): number[] {
  let col: number[] = [];
  let y: number = 0;
  while ((y < SIZE)) {
    col.push(b.cells[y][x]);
    y = (y + 1);
  }
  return col;
}
function setCol(b: Board, x: number, col: number[]) {
  let rows: number[][] = b.cells;
  let y: number = 0;
  while ((y < SIZE)) {
    let row: number[] = rows[y];
    row[x] = col[y];
    rows[y] = row;
    y = (y + 1);
  }
  b.cells = rows;
}
function moveUp(b: Board, score: number): MoveResult {
  let grid: number[][] = b.cells;
  let moved: boolean = false;
  let x: number = 0;
  while ((x < SIZE)) {
    let col: number[] = getCol(b, x);
    let r: SpawnResult = slideLeft(col);
    let _new = r.row;
    score = (score + r.gain);
    let y: number = 0;
    while ((y < SIZE)) {
      if ((grid[y][x] != _new[y])) {
        moved = true;
      }
      grid[y][x] = _new[y];
      y = (y + 1);
    }
    x = (x + 1);
  }
  return {"board": {"cells": grid}, score, moved};
}
function moveDown(b: Board, score: number): MoveResult {
  let grid: number[][] = b.cells;
  let moved: boolean = false;
  let x: number = 0;
  while ((x < SIZE)) {
    let col: number[] = reverseRow(getCol(b, x));
    let r: SpawnResult = slideLeft(col);
    col = r.row;
    score = (score + r.gain);
    col = reverseRow(col);
    let y: number = 0;
    while ((y < SIZE)) {
      if ((grid[y][x] != col[y])) {
        moved = true;
      }
      grid[y][x] = col[y];
      y = (y + 1);
    }
    x = (x + 1);
  }
  return {"board": {"cells": grid}, score, moved};
}
function hasMoves(b: Board): boolean {
  let y: number = 0;
  while ((y < SIZE)) {
    let x: number = 0;
    while ((x < SIZE)) {
      if ((b.cells[y][x] == 0)) {
        return true;
      }
      if ((((x + 1) < SIZE) && (b.cells[y][x] == b.cells[y][Math.trunc((x + 1))]))) {
        return true;
      }
      if ((((y + 1) < SIZE) && (b.cells[y][x] == b.cells[Math.trunc((y + 1))][x]))) {
        return true;
      }
      x = (x + 1);
    }
    y = (y + 1);
  }
  return false;
}
function has2048(b: Board): boolean {
  let y: number = 0;
  while ((y < SIZE)) {
    let x: number = 0;
    while ((x < SIZE)) {
      if ((b.cells[y][x] >= 2048)) {
        return true;
      }
      x = (x + 1);
    }
    y = (y + 1);
  }
  return false;
}
let score: number = 0;
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
let _inputData: string[] | null = null;
function _input(): string {
  if (_inputData === null) {
    let data: string;
    if (typeof Deno !== 'undefined') {
      const dec = new TextDecoder();
      const chunks: string[] = [];
      const buf = new Uint8Array(1024);
      for (;;) {
        const n = Deno.stdin.readSync(buf);
        if (n === null) break;
        chunks.push(dec.decode(buf.subarray(0, n)));
        if (n < buf.length) break;
      }
      data = chunks.join('');
    } else {
      const fs = require('fs');
      data = fs.readFileSync(0, 'utf8');
    }
    _inputData = data.split(/\r?\n/);
  }
  const v = _inputData.shift();
  return v === undefined ? '' : v;
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
  let board: Board = newBoard();
  let r: SpawnResult = spawnTile(board);
  board = r.board;
  let full: boolean = r.full;
  r = spawnTile(board);
  board = r.board;
  full = r.full;
  draw(board, score);
  while (true) {
    console.log(_str("Move: "));
    let cmd: string = _input();
    let moved: boolean = false;
    if (((cmd == "a") || (cmd == "A"))) {
      let m: MoveResult = moveLeft(board, score);
      board = m.board;
      score = m.score;
      moved = m.moved;
    }
    if (((cmd == "d") || (cmd == "D"))) {
      let m: MoveResult = moveRight(board, score);
      board = m.board;
      score = m.score;
      moved = m.moved;
    }
    if (((cmd == "w") || (cmd == "W"))) {
      let m: MoveResult = moveUp(board, score);
      board = m.board;
      score = m.score;
      moved = m.moved;
    }
    if (((cmd == "s") || (cmd == "S"))) {
      let m: MoveResult = moveDown(board, score);
      board = m.board;
      score = m.score;
      moved = m.moved;
    }
    if (((cmd == "q") || (cmd == "Q"))) {
      break
    }
    if (moved) {
      let r2: SpawnResult = spawnTile(board);
      board = r2.board;
      full = r2.full;
      if ((full && !hasMoves(board))) {
        draw(board, score);
        console.log(_str("Game Over"));
        break
      }
    }
    draw(board, score);
    if (has2048(board)) {
      console.log(_str("You win!"));
      break
    }
    if (!hasMoves(board)) {
      console.log(_str("Game Over"));
      break
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

