<?php
ini_set('memory_limit', '-1');
$b3Seg = 30;
function pixelFromRgb($rgb) {
  global $b3Seg, $newBitmap, $setPx, $fill, $fillRgb, $line, $bezier3;
  $r = intval(((intdiv($rgb, 65536)) % 256));
  $g = intval(((intdiv($rgb, 256)) % 256));
  $b = intval(($rgb % 256));
  return ['r' => $r, 'g' => $g, 'b' => $b];
}
function newBitmap($cols, $rows) {
  global $b3Seg, $pixelFromRgb, $setPx, $fill, $fillRgb, $line, $bezier3, $b;
  $d = [];
  $y = 0;
  while ($y < $rows) {
  $row = [];
  $x = 0;
  while ($x < $cols) {
  $row = array_merge($row, [['r' => 0, 'g' => 0, 'b' => 0]]);
  $x = $x + 1;
};
  $d = array_merge($d, [$row]);
  $y = $y + 1;
};
  return ['cols' => $cols, 'rows' => $rows, 'data' => $d];
}
function setPx($b, $x, $y, $p) {
  global $b3Seg, $pixelFromRgb, $newBitmap, $fill, $fillRgb, $line, $bezier3;
  $cols = intval($b['cols']);
  $rows = intval($b['rows']);
  if ($x >= 0 && $x < $cols && $y >= 0 && $y < $rows) {
  $b['data'][$y][$x] = $p;
}
}
function fill($b, $p) {
  global $b3Seg, $pixelFromRgb, $newBitmap, $setPx, $fillRgb, $line, $bezier3;
  $cols = intval($b['cols']);
  $rows = intval($b['rows']);
  $y = 0;
  while ($y < $rows) {
  $x = 0;
  while ($x < $cols) {
  $b['data'][$y][$x] = $p;
  $x = $x + 1;
};
  $y = $y + 1;
};
}
function fillRgb($b, $rgb) {
  global $b3Seg, $pixelFromRgb, $newBitmap, $setPx, $fill, $line, $bezier3;
  fill($b, pixelFromRgb($rgb));
}
function line($b, $x0, $y0, $x1, $y1, $p) {
  global $b3Seg, $pixelFromRgb, $newBitmap, $setPx, $fill, $fillRgb, $bezier3;
  $dx = $x1 - $x0;
  if ($dx < 0) {
  $dx = -$dx;
}
  $dy = $y1 - $y0;
  if ($dy < 0) {
  $dy = -$dy;
}
  $sx = -1;
  if ($x0 < $x1) {
  $sx = 1;
}
  $sy = -1;
  if ($y0 < $y1) {
  $sy = 1;
}
  $err = $dx - $dy;
  while (true) {
  setPx($b, $x0, $y0, $p);
  if ($x0 == $x1 && $y0 == $y1) {
  break;
}
  $e2 = 2 * $err;
  if ($e2 > (0 - $dy)) {
  $err = $err - $dy;
  $x0 = $x0 + $sx;
}
  if ($e2 < $dx) {
  $err = $err + $dx;
  $y0 = $y0 + $sy;
}
};
}
function bezier3($b, $x1, $y1, $x2, $y2, $x3, $y3, $x4, $y4, $p) {
  global $b3Seg, $pixelFromRgb, $newBitmap, $setPx, $fill, $fillRgb, $line;
  $px = [];
  $py = [];
  $i = 0;
  while ($i <= $b3Seg) {
  $px = array_merge($px, [0]);
  $py = array_merge($py, [0]);
  $i = $i + 1;
};
  $fx1 = floatval($x1);
  $fy1 = floatval($y1);
  $fx2 = floatval($x2);
  $fy2 = floatval($y2);
  $fx3 = floatval($x3);
  $fy3 = floatval($y3);
  $fx4 = floatval($x4);
  $fy4 = floatval($y4);
  $i = 0;
  while ($i <= $b3Seg) {
  $d = (floatval($i)) / (floatval($b3Seg));
  $a = 1.0 - $d;
  $bcoef = $a * $a;
  $ccoef = $d * $d;
  $a2 = $a * $bcoef;
  $b2 = 3.0 * $bcoef * $d;
  $c2 = 3.0 * $a * $ccoef;
  $d2 = $ccoef * $d;
  $px[$i] = intval(($a2 * $fx1 + $b2 * $fx2 + $c2 * $fx3 + $d2 * $fx4));
  $py[$i] = intval(($a2 * $fy1 + $b2 * $fy2 + $c2 * $fy3 + $d2 * $fy4));
  $i = $i + 1;
};
  $x0 = $px[0];
  $y0 = $py[0];
  $i = 1;
  while ($i <= $b3Seg) {
  $x = $px[$i];
  $y = $py[$i];
  line($b, $x0, $y0, $x, $y, $p);
  $x0 = $x;
  $y0 = $y;
  $i = $i + 1;
};
}
$b = newBitmap(400, 300);
fillRgb($b, 16773055);
bezier3($b, 20, 200, 700, 50, -300, 50, 380, 150, pixelFromRgb(4165615));
