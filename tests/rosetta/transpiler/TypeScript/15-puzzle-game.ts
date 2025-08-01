// Generated by Mochi v0.10.42 on 2025-07-28 10:39:50 GMT+7

let board: number[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
let solved: number[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
let empty: number = 15;
let moves: number = 0;
let quit: boolean = false;
function randMove(): number {
  return (_now() % 4);
}
function isSolved(): boolean {
  let i: number = 0;
  while ((i < 16)) {
    if ((board[i] != solved[i])) {
      return false;
    }
    i = (i + 1);
  }
  return true;
}
export interface MoveResult { idx: number; ok: boolean }
function isValidMove(m: number): MoveResult {
  if ((m == 0)) {
    return {"idx": (empty - 4), "ok": (Math.trunc(empty / 4) > 0)};
  }
  if ((m == 1)) {
    return {"idx": (empty + 4), "ok": (Math.trunc(empty / 4) < 3)};
  }
  if ((m == 2)) {
    return {"idx": (empty + 1), "ok": ((empty % 4) < 3)};
  }
  if ((m == 3)) {
    return {"idx": (empty - 1), "ok": ((empty % 4) > 0)};
  }
  return {"idx": 0, "ok": false};
}
function doMove(m: number): boolean {
  let r: MoveResult = isValidMove(m);
  if (!r.ok) {
    return false;
  }
  let i: number = empty;
  let j: number = r.idx;
  let tmp: number = board[i];
  board[i] = board[j];
  board[j] = tmp;
  empty = j;
  moves = (moves + 1);
  return true;
}
function shuffle(n: number) {
  let i: number = 0;
  while (((i < n) || isSolved())) {
    if (doMove(randMove())) {
      i = (i + 1);
    }
  }
}
function printBoard() {
  let line: string = "";
  let i: number = 0;
  while ((i < 16)) {
    let val: number = board[i];
    if ((val == 0)) {
      line = (line + "  .");
    } else {
      let s: string = _str(val);
      if ((val < 10)) {
        line = ((line + "  ") + s);
      } else {
        line = ((line + " ") + s);
      }
    }
    if (((i % 4) == 3)) {
      console.log(_str(line));
      line = "";
    }
    i = (i + 1);
  }
}
function playOneMove() {
  while (true) {
    console.log(_str((("Enter move #" + _str((moves + 1))) + " (U, D, L, R, or Q): ")));
    let s: string = _input();
    if ((s == "")) {
      continue
    }
    let c: string = s.slice(0, 1);
    let m: number = 0;
    if (((c == "U") || (c == "u"))) {
      m = 0;
    } else {
      if (((c == "D") || (c == "d"))) {
        m = 1;
      } else {
        if (((c == "R") || (c == "r"))) {
          m = 2;
        } else {
          if (((c == "L") || (c == "l"))) {
            m = 3;
          } else {
            if (((c == "Q") || (c == "q"))) {
              console.log(_str((("Quiting after " + _str(moves)) + " moves.")));
              quit = true;
              return;
            } else {
              console.log(_str(((("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" + "up, down, left, or right. You can also enter \"Q\" to quit.\n") + "Upper or lowercase is accepted and only the first non-blank\n") + "character is important (i.e. you may enter \"up\" if you like).")));
              continue
            }
          }
        }
      }
    }
    if (!doMove(m)) {
      console.log(_str("That is not a valid move at the moment."));
      continue
    }
    return;
  }
}
function play() {
  console.log(_str("Starting board:"));
  while ((!quit && (isSolved() == false))) {
    console.log(_str(""));
    printBoard();
    playOneMove();
  }
  if (isSolved()) {
    console.log(_str((("You solved the puzzle in " + _str(moves)) + " moves.")));
  }
}
function main() {
  shuffle(50);
  play();
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

