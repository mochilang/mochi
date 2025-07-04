// Generated by Mochi TypeScript compiler

function largestDivisibleSubset(nums: Array<number>): Array<number> {
  let n: number = nums.length;
  (globalThis as any).n = n;
  if ((n == 0)) {
    return [];
  }
  let sorted: Array<number> = (() => {
    const _src = nums;
    let _items = [];
    for (const x of _src) {
      _items.push(x);
    }
    let _pairs = _items.map((it) => {
      const x = it;
      return { item: it, key: x };
    });
    _pairs.sort((a, b) => {
      const ak = a.key;
      const bk = b.key;
      if (typeof ak === "number" && typeof bk === "number") return ak - bk;
      if (typeof ak === "string" && typeof bk === "string") {
        return ak < bk
          ? -1
          : (ak > bk ? 1 : 0);
      }
      return String(ak) < String(bk) ? -1 : (String(ak) > String(bk) ? 1 : 0);
    });
    _items = _pairs.map((p) => p.item);
    const _res = [];
    for (const x of _items) {
      _res.push(x);
    }
    return _res;
  })();
  (globalThis as any).sorted = sorted;
  let dp: Array<number> = [];
  (globalThis as any).dp = dp;
  let parent: Array<number> = [];
  (globalThis as any).parent = parent;
  let fill: number = 0;
  (globalThis as any).fill = fill;
  while ((fill < n)) {
    dp = dp.concat([1]);
    parent = parent.concat([-1]);
    fill = fill + 1;
  }
  let maxLen: number = 1;
  (globalThis as any).maxLen = maxLen;
  let maxIdx: number = 0;
  (globalThis as any).maxIdx = maxIdx;
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i < n)) {
    let j: number = 0;
    (globalThis as any).j = j;
    while ((j < i)) {
      if (((sorted[i] % sorted[j]) == 0)) {
        let candidate: number = dp[j] + 1;
        (globalThis as any).candidate = candidate;
        if ((candidate > dp[i])) {
          dp[i] = candidate;
          parent[i] = j;
        }
      }
      j = j + 1;
    }
    if ((dp[i] > maxLen)) {
      maxLen = dp[i];
      maxIdx = i;
    }
    i = i + 1;
  }
  let subset: Array<number> = [];
  (globalThis as any).subset = subset;
  let k: number = maxIdx;
  (globalThis as any).k = k;
  while ((k >= 0)) {
    subset = [sorted[k]].concat(subset);
    k = parent[k];
  }
  return subset;
}

function test_example_1(): void {
  if (
    !(_equal(
      largestDivisibleSubset([
        1,
        2,
        3,
      ]),
      [
        1,
        2,
      ],
    ))
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  if (
    !(_equal(
      largestDivisibleSubset([
        1,
        2,
        4,
        8,
      ]),
      [
        1,
        2,
        4,
        8,
      ],
    ))
  ) throw new Error("expect failed");
}

function test_empty(): void {
  if (!(_equal(largestDivisibleSubset([]), []))) {
    throw new Error("expect failed");
  }
}

function test_mixed_numbers(): void {
  if (
    !(_equal(
      largestDivisibleSubset([
        4,
        8,
        10,
        240,
      ]),
      [
        4,
        8,
        240,
      ],
    ))
  ) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_empty();
  test_mixed_numbers();
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
