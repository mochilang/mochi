<?php
ini_set('memory_limit', '-1');
function initGrid($size) {
  global $set, $circle, $trimRight;
  $g = [];
  $y = 0;
  while ($y < $size) {
  $row = [];
  $x = 0;
  while ($x < $size) {
  $row = array_merge($row, [' ']);
  $x = $x + 1;
};
  $g = array_merge($g, [$row]);
  $y = $y + 1;
};
  return $g;
}
function set(&$g, $x, $y) {
  global $initGrid, $circle, $trimRight;
  if ($x >= 0 && $x < count($g[0]) && $y >= 0 && $y < count($g)) {
  $g[$y][$x] = '#';
}
}
function circle($r) {
  global $initGrid, $set, $trimRight;
  $size = $r * 2 + 1;
  $g = initGrid($size);
  $x = $r;
  $y = 0;
  $err = 1 - $r;
  while ($y <= $x) {
  set($g, $r + $x, $r + $y);
  set($g, $r + $y, $r + $x);
  set($g, $r - $x, $r + $y);
  set($g, $r - $y, $r + $x);
  set($g, $r - $x, $r - $y);
  set($g, $r - $y, $r - $x);
  set($g, $r + $x, $r - $y);
  set($g, $r + $y, $r - $x);
  $y = $y + 1;
  if ($err < 0) {
  $err = $err + 2 * $y + 1;
} else {
  $x = $x - 1;
  $err = $err + 2 * ($y - $x) + 1;
}
};
  return $g;
}
function trimRight($row) {
  global $initGrid, $set, $circle, $g;
  $end = count($row);
  while ($end > 0 && $row[$end - 1] == ' ') {
  $end = $end - 1;
};
  $s = '';
  $i = 0;
  while ($i < $end) {
  $s = $s . $row[$i];
  $i = $i + 1;
};
  return $s;
}
$g = circle(10);
foreach ($g as $row) {
  echo rtrim(trimRight($row)), PHP_EOL;
}
