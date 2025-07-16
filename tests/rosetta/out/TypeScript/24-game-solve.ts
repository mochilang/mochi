// Source: /workspace/mochi/tests/rosetta/x/Mochi/24-game-solve.mochi

function newNum(n: number): Record<string, any> {
  return {
    "op": OP_NUM,
    "value": {
      "num": n,
      "denom": 1,
    },
  };
}

function exprEval(x: Record<string, any>): { [key: string]: number } {
  if ((x["op"] == OP_NUM)) {
    return x["value"];
  }
  let l = exprEval(x["left"]);
  let r = exprEval(x["right"]);
  if ((x["op"] == OP_ADD)) {
    return {
      "num": ((l["num"] * r["denom"]) + (l["denom"] * r["num"])),
      "denom": (l["denom"] * r["denom"]),
    };
  }
  if ((x["op"] == OP_SUB)) {
    return {
      "num": ((l["num"] * r["denom"]) - (l["denom"] * r["num"])),
      "denom": (l["denom"] * r["denom"]),
    };
  }
  if ((x["op"] == OP_MUL)) {
    return {
      "num": (l["num"] * r["num"]),
      "denom": (l["denom"] * r["denom"]),
    };
  }
  return {
    "num": (l["num"] * r["denom"]),
    "denom": (l["denom"] * r["num"]),
  };
}

function exprString(x: Record<string, any>): string {
  if ((x["op"] == OP_NUM)) {
    return String((x["value"] as any)["num"]);
  }
  let ls = exprString(x["left"]);
  let rs = exprString(x["right"]);
  var opstr = "";
  if ((x["op"] == OP_ADD)) {
    opstr = " + ";
  } else if ((x["op"] == OP_SUB)) {
    opstr = " - ";
  } else if ((x["op"] == OP_MUL)) {
    opstr = " * ";
  } else {
    opstr = " / ";
  }
  return `(${ls}${opstr}${rs})`;
}

function solve(xs: Record<string, any>[]): boolean {
  if ((xs.length == 1)) {
    let f = exprEval(xs[0]);
    if (((f["denom"] != 0) && (f["num"] == (f["denom"] * goal)))) {
      console.log(exprString(xs[0]));
      return true;
    }
    return false;
  }
  var i = 0;
  while ((i < xs.length)) {
    var j = i + 1;
    while ((j < xs.length)) {
      var rest: Record<string, any>[] = [];
      var k = 0;
      while ((k < xs.length)) {
        if (((k != i) && (k != j))) {
          rest = [...rest, xs[k]];
        }
        k = k + 1;
      }
      let a = xs[i];
      let b = xs[j];
      for (
        const op of [
          OP_ADD,
          OP_SUB,
          OP_MUL,
          OP_DIV,
        ]
      ) {
        var node = {
          "op": op,
          "left": a,
          "right": b,
        };
        if (solve([...rest, node])) {
          return true;
        }
      }
      var node = {
        "op": OP_SUB,
        "left": b,
        "right": a,
      };
      if (solve([...rest, node])) {
        return true;
      }
      node = {
        "op": OP_DIV,
        "left": b,
        "right": a,
      };
      if (solve([...rest, node])) {
        return true;
      }
      j = j + 1;
    }
    i = i + 1;
  }
  return false;
}

function main(): void {
  var iter = 0;
  while ((iter < 10)) {
    var cards: Record<string, any>[] = [];
    var i = 0;
    while ((i < n_cards)) {
      let n = (performance.now() * 1000000 % (digit_range - 1)) + 1;
      cards = [...cards, newNum(n)];
      console.log(` ${String(n)}`);
      i = i + 1;
    }
    console.log(":  ");
    if ((!solve(cards))) {
      console.log("No solution");
    }
    iter = iter + 1;
  }
}

let OP_ADD: number;
let OP_DIV: number;
let OP_MUL: number;
let OP_NUM: number;
let OP_SUB: number;
let digit_range: number;
let goal: number;
let n_cards: number;

let OP_NUM = 0;
let OP_ADD = 1;
let OP_SUB = 2;
let OP_MUL = 3;
let OP_DIV = 4;
let n_cards = 4;
let goal = 24;
let digit_range = 9;
main();
