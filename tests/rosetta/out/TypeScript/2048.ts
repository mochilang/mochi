// Source: /workspace/mochi/tests/rosetta/x/Mochi/2048.mochi

function newBoard(): number[][] {
  var b: number[][] = [];
  var y = 0;
  while ((y < SIZE)) {
    var row: number[] = [];
    var x = 0;
    while ((x < SIZE)) {
      row = [...row, 0];
      x = x + 1;
    }
    b = [...b, row];
    y = y + 1;
  }
  return b;
}

function spawnTile(b: number[][]): Record<string, any> {
  var empty: number[][] = [];
  var y = 0;
  while ((y < SIZE)) {
    var x = 0;
    while ((x < SIZE)) {
      if ((b[y][x] == 0)) {
        empty = [...empty, [
          x,
          y,
        ]];
      }
      x = x + 1;
    }
    y = y + 1;
  }
  if ((empty.length == 0)) {
    return {
      "board": b,
      "full": true,
    };
  }
  var idx = performance.now() * 1000000 % empty.length;
  let cell = empty[idx];
  var val = 4;
  if (((performance.now() * 1000000 % 10) < 9)) {
    val = 2;
  }
  b[cell[1]][cell[0]] = val;
  return {
    "board": b,
    "full": (empty.length == 1),
  };
}

function pad(n: number): string {
  var s = String(n);
  var pad = 4 - s.length;
  var i = 0;
  var out = "";
  while ((i < pad)) {
    out = `${out} `;
    i = i + 1;
  }
  return `${out}${s}`;
}

function draw(b: number[][], score: number): void {
  console.log(`Score: ${String(score)}`);
  var y = 0;
  while ((y < SIZE)) {
    console.log("+----+----+----+----+");
    var line = "|";
    var x = 0;
    while ((x < SIZE)) {
      var v = b[y][x];
      if ((v == 0)) {
        line = `${line}    |`;
      } else {
        line = `${line}${pad(v)}|`;
      }
      x = x + 1;
    }
    console.log(line);
    y = y + 1;
  }
  console.log("+----+----+----+----+");
  return console.log("W=Up S=Down A=Left D=Right Q=Quit");
}

function reverseRow(r: number[]): number[] {
  var out: number[] = [];
  var i = r.length - 1;
  while ((i >= 0)) {
    out = [...out, r[i]];
    i = i - 1;
  }
  return out;
}

function slideLeft(row: number[]): Record<string, any> {
  var xs: number[] = [];
  var i = 0;
  while ((i < row.length)) {
    if ((row[i] != 0)) {
      xs = [...xs, row[i]];
    }
    i = i + 1;
  }
  var res: number[] = [];
  var gain = 0;
  i = 0;
  while ((i < xs.length)) {
    if ((((i + 1) < xs.length) && (xs[i] == xs[i + 1]))) {
      let v = xs[i] * 2;
      gain = gain + v;
      res = [...res, v];
      i = i + 2;
    } else {
      res = [...res, xs[i]];
      i = i + 1;
    }
  }
  while ((res.length < SIZE)) {
    res = [...res, 0];
  }
  return {
    "row": res,
    "gain": gain,
  };
}

function moveLeft(b: number[][], score: number): Record<string, any> {
  var moved = false;
  var y = 0;
  while ((y < SIZE)) {
    let r = slideLeft(b[y]);
    let _new = r["row"];
    score = score + r["gain"];
    var x = 0;
    while ((x < SIZE)) {
      if ((b[y][x] != (_new as any)[x])) {
        moved = true;
      }
      b[y][x] = (_new as any)[x];
      x = x + 1;
    }
    y = y + 1;
  }
  return {
    "board": b,
    "score": score,
    "moved": moved,
  };
}

function moveRight(b: number[][], score: number): Record<string, any> {
  var moved = false;
  var y = 0;
  while ((y < SIZE)) {
    var rev = reverseRow(b[y]);
    let r = slideLeft(rev);
    rev = r["row"];
    score = score + r["gain"];
    rev = reverseRow(rev);
    var x = 0;
    while ((x < SIZE)) {
      if ((b[y][x] != rev[x])) {
        moved = true;
      }
      b[y][x] = rev[x];
      x = x + 1;
    }
    y = y + 1;
  }
  return {
    "board": b,
    "score": score,
    "moved": moved,
  };
}

function getCol(b: number[][], x: number): number[] {
  var col: number[] = [];
  var y = 0;
  while ((y < SIZE)) {
    col = [...col, b[y][x]];
    y = y + 1;
  }
  return col;
}

function setCol(b: number[][], x: number, col: number[]): void {
  var y = 0;
  while ((y < SIZE)) {
    b[y][x] = col[y];
    y = y + 1;
  }
}

function moveUp(b: number[][], score: number): Record<string, any> {
  var moved = false;
  var x = 0;
  while ((x < SIZE)) {
    var col = getCol(b, x);
    let r = slideLeft(col);
    let _new = r["row"];
    score = score + r["gain"];
    var y = 0;
    while ((y < SIZE)) {
      if ((b[y][x] != (_new as any)[y])) {
        moved = true;
      }
      b[y][x] = (_new as any)[y];
      y = y + 1;
    }
    x = x + 1;
  }
  return {
    "board": b,
    "score": score,
    "moved": moved,
  };
}

function moveDown(b: number[][], score: number): Record<string, any> {
  var moved = false;
  var x = 0;
  while ((x < SIZE)) {
    var col = reverseRow(getCol(b, x));
    let r = slideLeft(col);
    col = r["row"];
    score = score + r["gain"];
    col = reverseRow(col);
    var y = 0;
    while ((y < SIZE)) {
      if ((b[y][x] != col[y])) {
        moved = true;
      }
      b[y][x] = col[y];
      y = y + 1;
    }
    x = x + 1;
  }
  return {
    "board": b,
    "score": score,
    "moved": moved,
  };
}

function hasMoves(b: number[][]): boolean {
  var y = 0;
  while ((y < SIZE)) {
    var x = 0;
    while ((x < SIZE)) {
      if ((b[y][x] == 0)) {
        return true;
      }
      if ((((x + 1) < SIZE) && (b[y][x] == b[y][x + 1]))) {
        return true;
      }
      if ((((y + 1) < SIZE) && (b[y][x] == b[y + 1][x]))) {
        return true;
      }
      x = x + 1;
    }
    y = y + 1;
  }
  return false;
}

function has2048(b: number[][]): boolean {
  var y = 0;
  while ((y < SIZE)) {
    var x = 0;
    while ((x < SIZE)) {
      if ((b[y][x] >= 2048)) {
        return true;
      }
      x = x + 1;
    }
    y = y + 1;
  }
  return false;
}

let SIZE: number;
var board: number[][];
var full: any;
var r: Record<string, any>;
var score: number;

function main(): void {
  SIZE = 4;
  board = newBoard();
  r = spawnTile(board);
  board = r["board"];
  full = r["full"];
  r = spawnTile(board);
  board = r["board"];
  full = r["full"];
  score = 0;
  draw(board, score);
  while (true) {
    console.log("Move: ");
    let cmd = _input();
    var moved = false;
    if (((cmd == "a") || (cmd == "A"))) {
      let m = moveLeft(board, score);
      board = m["board"];
      score = m["score"];
      moved = m["moved"];
    }
    if (((cmd == "d") || (cmd == "D"))) {
      let m = moveRight(board, score);
      board = m["board"];
      score = m["score"];
      moved = m["moved"];
    }
    if (((cmd == "w") || (cmd == "W"))) {
      let m = moveUp(board, score);
      board = m["board"];
      score = m["score"];
      moved = m["moved"];
    }
    if (((cmd == "s") || (cmd == "S"))) {
      let m = moveDown(board, score);
      board = m["board"];
      score = m["score"];
      moved = m["moved"];
    }
    if (((cmd == "q") || (cmd == "Q"))) {
      break;
    }
    if (moved) {
      let r2 = spawnTile(board);
      board = r2["board"];
      full = r2["full"];
      if ((full && (!hasMoves(board)))) {
        draw(board, score);
        console.log("Game Over");
        break;
      }
    }
    draw(board, score);
    if (has2048(board)) {
      console.log("You win!");
      break;
    }
    if ((!hasMoves(board))) {
      console.log("Game Over");
      break;
    }
  }
}
function _input(): string {
  const v = prompt("");
  return v === null ? "" : v;
}

main();
