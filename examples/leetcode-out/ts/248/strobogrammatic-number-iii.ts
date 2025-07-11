// Generated by Mochi TypeScript compiler

function helper(n: number, total: number): Array<string> {
  if ((n == 0)) {
    return [""];
  }
  if ((n == 1)) {
    return [
      "0",
      "1",
      "8",
    ];
  }
  let prev: Array<string> = helper(n - 2, total);
  (globalThis as any).prev = prev;
  let result: Array<string> = [];
  (globalThis as any).result = result;
  for (const x of prev) {
    if ((n != total)) {
      result = result.concat(["0" + x + "0"]);
    }
    result = result.concat(["1" + x + "1"]);
    result = result.concat(["6" + x + "9"]);
    result = result.concat(["8" + x + "8"]);
    result = result.concat(["9" + x + "6"]);
  }
  return result;
}

function strobogrammaticInRange(low: string, high: string): number {
  let m: number = low.length;
  (globalThis as any).m = m;
  let n: number = high.length;
  (globalThis as any).n = n;
  let count: number = 0;
  (globalThis as any).count = count;
  let len: number = m;
  (globalThis as any).len = len;
  while ((len <= n)) {
    let nums: Array<string> = helper(len, len);
    (globalThis as any).nums = nums;
    for (const num of nums) {
      if (((len == m) && (num < low))) {
        continue;
      }
      if (((len == n) && (num > high))) {
        continue;
      }
      if (((len > 1) && (_indexString(num, 0) == "0"))) {
        continue;
      }
      count = count + 1;
    }
    len = len + 1;
  }
  return count;
}

function test_example_1(): void {
  if (!(strobogrammaticInRange("50", "100") == 3)) {
    throw new Error("expect failed");
  }
}

function test_example_2(): void {
  if (!(strobogrammaticInRange("0", "0") == 1)) {
    throw new Error("expect failed");
  }
}

function test_example_3(): void {
  if (!(strobogrammaticInRange("2", "3") == 0)) {
    throw new Error("expect failed");
  }
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
