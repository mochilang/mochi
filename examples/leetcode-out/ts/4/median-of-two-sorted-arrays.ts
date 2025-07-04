// Generated by Mochi TypeScript compiler

function findMedianSortedArrays(
  nums1: Array<number>,
  nums2: Array<number>,
): number {
  let merged: Array<number> = [];
  (globalThis as any).merged = merged;
  let i: number = 0;
  (globalThis as any).i = i;
  let j: number = 0;
  (globalThis as any).j = j;
  while (((i < nums1.length) || (j < nums2.length))) {
    if ((j >= nums2.length)) {
      merged = merged.concat([nums1[i]]);
      i = i + 1;
    } else if ((i >= nums1.length)) {
      merged = merged.concat([nums2[j]]);
      j = j + 1;
    } else if ((nums1[i] <= nums2[j])) {
      merged = merged.concat([nums1[i]]);
      i = i + 1;
    } else {
      merged = merged.concat([nums2[j]]);
      j = j + 1;
    }
  }
  let total: number = merged.length;
  (globalThis as any).total = total;
  if (((total % 2) == 1)) {
    return merged[Math.trunc(total / 2)];
  }
  let mid1: number = merged[Math.trunc(total / 2) - 1];
  (globalThis as any).mid1 = mid1;
  let mid2: number = merged[Math.trunc(total / 2)];
  (globalThis as any).mid2 = mid2;
  return ((mid1 + mid2) / 2);
}

function test_example_1(): void {
  if (
    !(findMedianSortedArrays([
      1,
      3,
    ], [2]) == 2)
  ) throw new Error("expect failed");
}

function test_example_2(): void {
  if (
    !(findMedianSortedArrays([
      1,
      2,
    ], [
      3,
      4,
    ]) == 2.5)
  ) throw new Error("expect failed");
}

function test_empty_first(): void {
  if (!(findMedianSortedArrays([], [1]) == 1)) throw new Error("expect failed");
}

function test_empty_second(): void {
  if (!(findMedianSortedArrays([2], []) == 2)) throw new Error("expect failed");
}

function main(): void {
  test_example_1();
  test_example_2();
  test_empty_first();
  test_empty_second();
}
main();
