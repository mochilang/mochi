<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
$w = 400;
$h = 300;
$n = 15000;
$frost = 255;
$grid = [];
$y = 0;
while ($y < $h) {
  $row = [];
  $x = 0;
  while ($x < $w) {
  $row = array_merge($row, [0]);
  $x = $x + 1;
};
  $grid = array_merge($grid, [$row]);
  $y = $y + 1;
}
$grid[intdiv($h, 3)][intdiv($w, 3)] = $frost;
function inBounds($x, $y) {
  global $w, $h, $n, $frost, $grid, $row, $hasNeighbor, $a, $px, $py, $lost;
  return $x >= 0 && $x < $w && $y >= 0 && $y < $h;
}
function hasNeighbor($x, $y) {
  global $w, $h, $n, $frost, $grid, $row, $inBounds, $a, $px, $py, $lost;
  $dy = -1;
  while ($dy <= 1) {
  $dx = -1;
  while ($dx <= 1) {
  if (!($dx == 0 && $dy == 0)) {
  $nx = $x + $dx;
  $ny = $y + $dy;
  if (inBounds($nx, $ny) && $grid[$ny][$nx] == $frost) {
  return true;
};
}
  $dx = $dx + 1;
};
  $dy = $dy + 1;
};
  return false;
}
$a = 0;
while ($a < $n) {
  $px = _now() % $w;
  $py = _now() % $h;
  if ($grid[$py][$px] == $frost) {
  $lost = false;
  while (true) {
  $px = $px + (_now() % 3) - 1;
  $py = $py + (_now() % 3) - 1;
  if (!inBounds($px, $py)) {
  $lost = true;
  break;
}
  if ($grid[$py][$px] != $frost) {
  break;
}
};
  if ($lost) {
  continue;
};
} else {
  $lost = false;
  while (!hasNeighbor($px, $py)) {
  $px = $px + (_now() % 3) - 1;
  $py = $py + (_now() % 3) - 1;
  if (!inBounds($px, $py)) {
  $lost = true;
  break;
}
};
  if ($lost) {
  continue;
};
}
  $grid[$py][$px] = $frost;
  $a = $a + 1;
}
