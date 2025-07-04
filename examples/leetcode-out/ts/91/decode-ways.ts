// Generated by Mochi TypeScript compiler

function numDecodings(s: string): number {
  let n: number = s.length;
  (globalThis as any).n = n;
  if ((n == 0)) {
    return 0;
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
  let dp: Array<number> = [];
  (globalThis as any).dp = dp;
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i <= n)) {
    dp = dp.concat([0]);
    i = i + 1;
  }
  dp[0] = 1;
  if ((_indexString(s, 0) != "0")) {
    dp[1] = 1;
  }
  let idx: number = 2;
  (globalThis as any).idx = idx;
  while ((idx <= n)) {
    let one: string = _indexString(s, idx - 1);
    (globalThis as any).one = one;
    if ((one != "0")) {
      dp[idx] = dp[idx] + dp[idx - 1];
    }
    let d1: number = digits[_indexString(s, idx - 2)];
    (globalThis as any).d1 = d1;
    let d2: number = digits[_indexString(s, idx - 1)];
    (globalThis as any).d2 = d2;
    let val: number = (d1 * 10) + d2;
    (globalThis as any).val = val;
    if (((val >= 10) && (val <= 26))) {
      dp[idx] = dp[idx] + dp[idx - 2];
    }
    idx = idx + 1;
  }
  return dp[n];
}

function test_example_1(): void {
  if (!(numDecodings("12") == 2)) throw new Error("expect failed");
}

function test_example_2(): void {
  if (!(numDecodings("226") == 3)) throw new Error("expect failed");
}

function test_example_3(): void {
  if (!(numDecodings("06") == 0)) throw new Error("expect failed");
}

function test_single_zero(): void {
  if (!(numDecodings("0") == 0)) throw new Error("expect failed");
}

function test__101(): void {
  if (!(numDecodings("2101") == 1)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_example_3();
  test_single_zero();
  test__101();
}
function _indexString(s: string, i: number): string {
  const runes = Array.from(s);
  if (i < 0) i += runes.length;
  if (i < 0 || i >= runes.length) throw new Error("index out of range");
  return runes[i];
}

main();
