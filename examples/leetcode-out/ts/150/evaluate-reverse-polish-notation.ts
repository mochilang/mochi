// Generated by Mochi TypeScript compiler

function parseInt(s: string): number {
  let i: number = 0;
  (globalThis as any).i = i;
  let sign: number = 1;
  (globalThis as any).sign = sign;
  if (
    ((s.length > 0) &&
      ((_indexString(s, 0) == "-") || (_indexString(s, 0) == "+")))
  ) {
    if ((_indexString(s, 0) == "-")) {
      sign = -1;
    }
    i = 1;
  }
  let digits: Record<string, number> = {
    "0": 0,
    "1": 1,
    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5,
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9,
  };
  (globalThis as any).digits = digits;
  let result: number = 0;
  (globalThis as any).result = result;
  while ((i < s.length)) {
    let ch: string = _indexString(s, i);
    (globalThis as any).ch = ch;
    result = (result * 10) + digits[ch];
    i = i + 1;
  }
  return (result * sign);
}

function evalRPN(tokens: Array<string>): number {
  let stack: Array<number> = [];
  (globalThis as any).stack = stack;
  for (const tok of tokens) {
    if (((((tok == "+") || (tok == "-")) || (tok == "*")) || (tok == "/"))) {
      let b: number = stack[stack.length - 1];
      (globalThis as any).b = b;
      stack = stack.slice(0, stack.length - 1);
      let a: number = stack[stack.length - 1];
      (globalThis as any).a = a;
      stack = stack.slice(0, stack.length - 1);
      if ((tok == "+")) {
        stack = stack.concat([a + b]);
      } else if ((tok == "-")) {
        stack = stack.concat([a - b]);
      } else if ((tok == "*")) {
        stack = stack.concat([a * b]);
      } else {
        stack = stack.concat([Math.trunc(a / b)]);
      }
    } else {
      let val: number = parseInt(tok);
      (globalThis as any).val = val;
      stack = stack.concat([val]);
    }
  }
  return stack[stack.length - 1];
}

function test_example_1(): void {
  if (
    !(evalRPN([
      "2",
      "1",
      "+",
      "3",
      "*",
    ]) == 9)
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  if (
    !(evalRPN([
      "4",
      "13",
      "5",
      "/",
      "+",
    ]) == 6)
  ) throw new Error("expect failed");
}

function test_example_3(): void {
  if (
    !(evalRPN([
      "10",
      "6",
      "9",
      "3",
      "+",
      "-11",
      "*",
      "/",
      "*",
      "17",
      "+",
      "5",
      "+",
    ]) == 22)
  ) throw new Error("expect failed");
}

function test_single_number(): void {
  if (!(evalRPN(["42"]) == 42)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_single_number();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
