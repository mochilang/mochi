// Generated by Mochi TypeScript compiler

function verifyPreorder(preorder: Array<number>): boolean {
  let stack: Array<number> = [];
  (globalThis as any).stack = stack;
  let lower: number = -2147483648;
  (globalThis as any).lower = lower;
  for (const value of preorder) {
    if ((value < lower)) {
      return false;
    }
    while ((stack.length > 0)) {
      let top: number = stack[stack.length - 1];
      (globalThis as any).top = top;
      if ((value > top)) {
        lower = top;
        stack = stack.slice(0, stack.length - 1);
      } else {
        break;
      }
    }
    stack = stack.concat([value]);
  }
  return true;
}

function test_example_1(): void {
  if (
    !(verifyPreorder([
      5,
      2,
      1,
      3,
      6,
    ]) == true)
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  if (
    !(verifyPreorder([
      5,
      2,
      6,
      1,
      3,
    ]) == false)
  ) throw new Error("expect failed");
}

function test_single_node(): void {
  if (!(verifyPreorder([1]) == true)) throw new Error("expect failed");
}

function test_empty_list(): void {
  if (!(verifyPreorder([]) == true)) throw new Error("expect failed");
}

function test_strictly_increasing(): void {
  if (
    !(verifyPreorder([
      1,
      2,
      3,
      4,
      5,
    ]) == true)
  ) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_single_node();
  test_empty_list();
  test_strictly_increasing();
}
main();
