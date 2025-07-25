// Generated by Mochi TypeScript compiler

function getPermutation(n: number, k: number): string {
  let factVal: number = 1;
  (globalThis as any).factVal = factVal;
  for (let i: number = 1; i < (n + 1); i++) {
    factVal = factVal * i;
  }
  let nums: Array<number> = [];
  (globalThis as any).nums = nums;
  let i: number = 1;
  (globalThis as any).i = i;
  while ((i <= n)) {
    nums = nums.concat([i]);
    i = i + 1;
  }
  let k0: number = k - 1;
  (globalThis as any).k0 = k0;
  let result: string = "";
  (globalThis as any).result = result;
  i = 0;
  while ((i < n)) {
    factVal = Math.trunc(factVal / (n - i));
    let idx: number = Math.trunc(k0 / factVal);
    (globalThis as any).idx = idx;
    let digit: number = nums[idx];
    (globalThis as any).digit = digit;
    result = result + String(digit);
    nums = nums.slice(0, idx).concat(nums.slice(idx + 1, nums.length));
    k0 = k0 % factVal;
    i = i + 1;
  }
  return result;
}

function test_example_1(): void {
  if (!(getPermutation(3, 3) == "213")) throw new Error("expect failed");
}

function test_example_2(): void {
  if (!(getPermutation(4, 9) == "2314")) throw new Error("expect failed");
}

function test_example_3(): void {
  if (!(getPermutation(3, 1) == "123")) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
}
main();
