<?php
ini_set('memory_limit', '-1');
function absi($x) {
  global $bresenham, $main;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function bresenham($x0, $y0, $x1, $y1) {
  global $absi, $main;
  $dx = absi($x1 - $x0);
  $dy = absi($y1 - $y0);
  $sx = -1;
  if ($x0 < $x1) {
  $sx = 1;
}
  $sy = -1;
  if ($y0 < $y1) {
  $sy = 1;
}
  $err = $dx - $dy;
  $pts = [];
  while (true) {
  $pts = array_merge($pts, [['x' => $x0, 'y' => $y0]]);
  if ($x0 == $x1 && $y0 == $y1) {
  break;
}
  $e2 = 2 * $err;
  if ($e2 > (-$dy)) {
  $err = $err - $dy;
  $x0 = $x0 + $sx;
}
  if ($e2 < $dx) {
  $err = $err + $dx;
  $y0 = $y0 + $sy;
}
};
  return $pts;
}
function main() {
  global $absi, $bresenham;
  $pts = bresenham(0, 0, 6, 4);
  $i = 0;
  while ($i < count($pts)) {
  $p = $pts[$i];
  echo rtrim('(' . json_encode($p['x'], 1344) . ',' . json_encode($p['y'], 1344) . ')'), PHP_EOL;
  $i = $i + 1;
};
}
main();
