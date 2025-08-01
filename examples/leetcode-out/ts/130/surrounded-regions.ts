// Generated by Mochi TypeScript compiler

function solve(board: Array<Array<string>>): Array<Array<string>> {
  let rows: number = board.length;
  (globalThis as any).rows = rows;
  if ((rows == 0)) {
    return board;
  }
  let cols: number = board[0].length;
  (globalThis as any).cols = cols;
  let queue: Array<Array<number>> = [];
  (globalThis as any).queue = queue;
  for (let r: number = 0; r < rows; r++) {
    if ((board[r][0] == "O")) {
      queue = queue.concat([
        [
          r,
          0,
        ],
      ]);
    }
    if (((cols > 1) && (board[r][cols - 1] == "O"))) {
      queue = queue.concat([
        [
          r,
          cols - 1,
        ],
      ]);
    }
  }
  for (let c: number = 0; c < cols; c++) {
    if ((board[0][c] == "O")) {
      queue = queue.concat([
        [
          0,
          c,
        ],
      ]);
    }
    if (((rows > 1) && (board[rows - 1][c] == "O"))) {
      queue = queue.concat([
        [
          rows - 1,
          c,
        ],
      ]);
    }
  }
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i < queue.length)) {
    let pos: Array<number> = queue[i];
    (globalThis as any).pos = pos;
    let r: number = pos[0];
    (globalThis as any).r = r;
    let c: number = pos[1];
    (globalThis as any).c = c;
    if ((board[r][c] == "O")) {
      board[r][c] = "S";
      if ((r > 0)) {
        if ((board[r - 1][c] == "O")) {
          queue = queue.concat([
            [
              r - 1,
              c,
            ],
          ]);
        }
      }
      if (((r + 1) < rows)) {
        if ((board[r + 1][c] == "O")) {
          queue = queue.concat([
            [
              r + 1,
              c,
            ],
          ]);
        }
      }
      if ((c > 0)) {
        if ((board[r][c - 1] == "O")) {
          queue = queue.concat([
            [
              r,
              c - 1,
            ],
          ]);
        }
      }
      if (((c + 1) < cols)) {
        if ((board[r][c + 1] == "O")) {
          queue = queue.concat([
            [
              r,
              c + 1,
            ],
          ]);
        }
      }
    }
    i = i + 1;
  }
  for (let r: number = 0; r < rows; r++) {
    for (let c: number = 0; c < cols; c++) {
      if ((board[r][c] == "O")) {
        board[r][c] = "X";
      } else if ((board[r][c] == "S")) {
        board[r][c] = "O";
      }
    }
  }
  return board;
}

function test_example_1(): void {
  let board: Array<Array<string>> = [
    [
      "X",
      "X",
      "X",
      "X",
    ],
    [
      "X",
      "O",
      "O",
      "X",
    ],
    [
      "X",
      "X",
      "O",
      "X",
    ],
    [
      "X",
      "O",
      "X",
      "X",
    ],
  ];
  (globalThis as any).board = board;
  let expected: Array<Array<string>> = [
    [
      "X",
      "X",
      "X",
      "X",
    ],
    [
      "X",
      "X",
      "X",
      "X",
    ],
    [
      "X",
      "X",
      "X",
      "X",
    ],
    [
      "X",
      "O",
      "X",
      "X",
    ],
  ];
  (globalThis as any).expected = expected;
  if (!(_equal(solve(board), expected))) throw new Error("expect failed");
}

function test_no_change(): void {
  let board: Array<Array<string>> = [
    [
      "X",
      "X",
    ],
    [
      "X",
      "X",
    ],
  ];
  (globalThis as any).board = board;
  if (!(_equal(solve(board), board))) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_no_change();
}
function _equal(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

main();
