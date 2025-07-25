// Generated by Mochi v0.10.40 on 2025-07-25 17:41:41 GMT+7

const b2Seg: number = 20;
export interface Pixel { r: number; g: number; b: number }
function pixelFromRgb(rgb: number): Pixel {
  const r: number = Math.trunc((Math.trunc(rgb / 65536) % 256));
  const g: number = Math.trunc((Math.trunc(rgb / 256) % 256));
  const b: Record<string, any> = Math.trunc((rgb % 256));
  return {r, g, b};
}
function newBitmap(cols: number, rows: number): Record<string, any> {
  let d: Pixel[][] = [];
  let y: number = 0;
  while ((y < rows)) {
    let row: Pixel[] = [];
    let x: number = 0;
    while ((x < cols)) {
      row = [...row, {"r": 0, "g": 0, "b": 0}];
      x = (x + 1);
    }
    d = [...d, row];
    y = (y + 1);
  }
  return {cols, rows, "data": d};
}
function setPx(b: Record<string, any>, x: number, y: number, p: Pixel) {
  const cols: number = Math.trunc(b.cols);
  const rows: number = Math.trunc(b.rows);
  if (((((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows))) {
    b.data[y][x] = p;
  }
}
function fill(b: Record<string, any>, p: Pixel) {
  const cols: number = Math.trunc(b.cols);
  const rows: number = Math.trunc(b.rows);
  let y: number = 0;
  while ((y < rows)) {
    let x: number = 0;
    while ((x < cols)) {
      b.data[y][x] = p;
      x = (x + 1);
    }
    y = (y + 1);
  }
}
function fillRgb(b: Record<string, any>, rgb: number) {
  fill(b, pixelFromRgb(rgb));
}
function line(b: Record<string, any>, x0: number, y0: number, x1: number, y1: number, p: Pixel) {
  let dx: number = (x1 - x0);
  if ((dx < 0)) {
    dx = -dx;
  }
  let dy: number = (y1 - y0);
  if ((dy < 0)) {
    dy = -dy;
  }
  let sx: number = -1;
  if ((x0 < x1)) {
    sx = 1;
  }
  let sy: number = -1;
  if ((y0 < y1)) {
    sy = 1;
  }
  let err: number = (dx - dy);
  while (true) {
    setPx(b, x0, y0, p);
    if (((x0 == x1) && (y0 == y1))) {
      break
    }
    const e2: number = (2 * err);
    if ((e2 > (0 - dy))) {
      err = (err - dy);
      x0 = (x0 + sx);
    }
    if ((e2 < dx)) {
      err = (err + dx);
      y0 = (y0 + sy);
    }
  }
}
function bezier2(b: Record<string, any>, x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, p: Pixel) {
  let px: number[] = [];
  let py: number[] = [];
  let i: number = 0;
  while ((i <= b2Seg)) {
    px = [...px, 0];
    py = [...py, 0];
    i = (i + 1);
  }
  const fx1: number = x1;
  const fy1: number = y1;
  const fx2: number = x2;
  const fy2: number = y2;
  const fx3: number = x3;
  const fy3: number = y3;
  i = 0;
  while ((i <= b2Seg)) {
    const c: number = (i / b2Seg);
    let a: number = (1.0 - c);
    let a2: number = (a * a);
    let b2: number = ((2.0 * c) * a);
    let c2: number = (c * c);
    px[i] = Math.trunc((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)));
    py[i] = Math.trunc((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)));
    i = (i + 1);
  }
  let x0: number = px[Math.trunc(0)];
  let y0: number = py[Math.trunc(0)];
  i = 1;
  while ((i <= b2Seg)) {
    const x: number = px[i];
    const y: number = py[i];
    line(b, x0, y0, x, y, p);
    x0 = x;
    y0 = y;
    i = (i + 1);
  }
}
let b: Record<string, any> = newBitmap(400, 300);
fillRgb(b, 14614575);
bezier2(b, 20, 150, 500, -100, 300, 280, pixelFromRgb(4165615));
