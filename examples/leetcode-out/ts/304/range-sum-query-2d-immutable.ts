// Generated by Mochi TypeScript compiler

type NumMatrix = {
  sums: Array<Array<number>>;
};

function newNumMatrix(matrix: Array<Array<number>>): NumMatrix {
  let rows: number = matrix.length;
  (globalThis as any).rows = rows;
  if ((rows == 0)) {
    return { sums: [] };
  }
  let cols: number = matrix[0].length;
  (globalThis as any).cols = cols;
  let sums: Array<Array<number>> = [];
  (globalThis as any).sums = sums;
  let r: number = 0;
  (globalThis as any).r = r;
  while ((r <= rows)) {
    let row: Array<number> = [];
    (globalThis as any).row = row;
    let c: number = 0;
    (globalThis as any).c = c;
    while ((c <= cols)) {
      row = row.concat([0]);
      c = c + 1;
    }
    sums = sums.concat([row]);
    r = r + 1;
  }
  r = 1;
  while ((r <= rows)) {
    let c: number = 1;
    (globalThis as any).c = c;
    while ((c <= cols)) {
      let val: number = matrix[r - 1][c - 1];
      (globalThis as any).val = val;
      sums[r][c] = ((sums[r - 1][c] + sums[r][c - 1]) - sums[r - 1][c - 1]) +
        val;
      c = c + 1;
    }
    r = r + 1;
  }
  return { sums: sums };
}

function sumRegion(
  nm: NumMatrix,
  row1: number,
  col1: number,
  row2: number,
  col2: number,
): number {
  let s: Array<Array<number>> = nm.sums;
  (globalThis as any).s = s;
  return (((s[row2 + 1][col2 + 1] - s[row1][col2 + 1]) - s[row2 + 1][col1]) +
    s[row1][col1]);
}

function test_example(): void {
  let nm: NumMatrix = newNumMatrix([
    [
      3,
      0,
      1,
      4,
      2,
    ],
    [
      5,
      6,
      3,
      2,
      1,
    ],
    [
      1,
      2,
      0,
      1,
      5,
    ],
    [
      4,
      1,
      0,
      1,
      7,
    ],
    [
      1,
      0,
      3,
      0,
      5,
    ],
  ]);
  (globalThis as any).nm = nm;
  if (!(sumRegion(nm, 2, 1, 4, 3) == 8)) throw new Error("expect failed");
  if (!(sumRegion(nm, 1, 1, 2, 2) == 11)) throw new Error("expect failed");
  if (!(sumRegion(nm, 1, 2, 2, 4) == 12)) throw new Error("expect failed");
}

function main(): void {
  test_example();
}
main();
