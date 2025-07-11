// Generated by Mochi TypeScript compiler

type MedianFinder = {
  values: Array<number>;
};

function newFinder(): MedianFinder {
  return { values: [] };
}

function addNum(mf: MedianFinder, num: number): MedianFinder {
  let vals: Array<number> = mf.values;
  (globalThis as any).vals = vals;
  let i: number = 0;
  (globalThis as any).i = i;
  while ((i < vals.length)) {
    if ((vals[i] < num)) {
      i = i + 1;
    } else {
      break;
    }
  }
  vals = vals.slice(0, i).concat([num]).concat(vals.slice(i, vals.length));
  return { values: vals };
}

function findMedian(mf: MedianFinder): number {
  let n: number = mf.values.length;
  (globalThis as any).n = n;
  if (((n % 2) == 1)) {
    return (mf.values[Math.trunc(n / 2)]);
  }
  let a: number = mf.values[Math.trunc(n / 2) - 1];
  (globalThis as any).a = a;
  let b: number = mf.values[Math.trunc(n / 2)];
  (globalThis as any).b = b;
  return ((a + b) / 2);
}

function test_example(): void {
  let mf: MedianFinder = newFinder();
  (globalThis as any).mf = mf;
  mf = addNum(mf, 1);
  mf = addNum(mf, 2);
  if (!(findMedian(mf) == 1.5)) throw new Error("expect failed");
  mf = addNum(mf, 3);
  if (!(findMedian(mf) == 2)) throw new Error("expect failed");
}

function test_single_value(): void {
  let mf: MedianFinder = newFinder();
  (globalThis as any).mf = mf;
  mf = addNum(mf, 5);
  if (!(findMedian(mf) == 5)) throw new Error("expect failed");
}

function test_even_count(): void {
  let mf: MedianFinder = newFinder();
  (globalThis as any).mf = mf;
  mf = addNum(mf, 2);
  mf = addNum(mf, 4);
  mf = addNum(mf, 6);
  mf = addNum(mf, 8);
  if (!(findMedian(mf) == 5)) throw new Error("expect failed");
}

function main(): void {
  test_example();
  test_single_value();
  test_even_count();
}
main();
