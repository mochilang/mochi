// Source: /workspace/mochi/tests/rosetta/x/Mochi/24-game.mochi

function randDigit(): number {
  return ((_now() % 9) + 1);
}

function main(): void {
  var digits = [];
  for (let i: number = 0; i < 4; i++) {
    digits = [...digits, randDigit()];
  }
  var numstr = "";
  for (let i: number = 0; i < 4; i++) {
    numstr = `${numstr}${String(digits[i])}`;
  }
  console.log(`Your numbers: ${numstr}
`);
  console.log("Enter RPN: ");
  var expr = _input();
  if ((expr.length != 7)) {
    console.log(
      "invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)",
    );
    return undefined;
  }
  var stack = [];
  var i = 0;
  var valid = true;
  while ((i < expr.length)) {
    let ch = expr.substring(i, i + (i + 1));
    if (((ch >= "0") && (ch <= "9"))) {
      if ((digits.length == 0)) {
        console.log("too many numbers.");
        return undefined;
      }
      var j = 0;
      while ((digits[j] != (int(ch) - 0))) {
        j = j + 1;
        if ((j == digits.length)) {
          console.log("wrong numbers.");
          return undefined;
        }
      }
      digits = digits.slice(0, j).concat(digits.slice(j + 1, digits.length));
      stack = [...stack, float(int(ch) - 0)];
    } else {
      if ((stack.length < 2)) {
        console.log("invalid expression syntax.");
        valid = false;
        break;
      }
      var b = stack[stack.length - 1];
      var a = stack[stack.length - 2];
      if ((ch == "+")) {
        stack[stack.length - 2] = a + b;
      } else if ((ch == "-")) {
        stack[stack.length - 2] = a - b;
      } else if ((ch == "*")) {
        stack[stack.length - 2] = a * b;
      } else if ((ch == "/")) {
        stack[stack.length - 2] = a / b;
      } else {
        console.log(`${ch} invalid.`);
        valid = false;
        break;
      }
      stack = stack.slice(0, stack.length - 1);
    }
    i = i + 1;
  }
  if (valid) {
    if ((abs(stack[0] - 24) > 1e-06)) {
      console.log(`incorrect. ${String(stack[0])} != 24`);
    } else {
      console.log("correct.");
    }
  }
}

main();
var _nowSeed = 0;
var _nowSeeded = false;
{
  const s = Deno.env.get("MOCHI_NOW_SEED");
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
  return performance.now() * 1000000;
}
function _input(): string {
  const v = prompt("");
  return v === null ? "" : v;
}
