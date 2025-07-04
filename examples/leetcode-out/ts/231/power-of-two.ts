// Generated by Mochi TypeScript compiler

function isPowerOfTwo(n: number): boolean {
  if ((n <= 0)) {
    return false;
  }
  let num: number = n;
  (globalThis as any).num = num;
  while ((num > 1)) {
    if (((num % 2) != 0)) {
      return false;
    }
    num = Math.trunc(num / 2);
  }
  return true;
}

function test_example_1(): void {
  if (!(isPowerOfTwo(1) == true)) throw new Error("expect failed");
}

function test_example_2(): void {
  if (!(isPowerOfTwo(16) == true)) throw new Error("expect failed");
}

function test_example_3(): void {
  if (!(isPowerOfTwo(3) == false)) throw new Error("expect failed");
}

function test_zero(): void {
  if (!(isPowerOfTwo(0) == false)) throw new Error("expect failed");
}

function test_negative(): void {
  if (!(isPowerOfTwo(-2) == false)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_zero();
  test_negative();
}
main();
