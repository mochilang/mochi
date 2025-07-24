<?php
ini_set('memory_limit', '-1');
$width = 60;
$height = intval((floatval($width) * 0.86602540378));
$iterations = 5000;
$grid = [];
$y = 0;
while ($y < $height) {
  $line = [];
  $x = 0;
  while ($x < $width) {
  $line = array_merge($line, [' ']);
  $x = $x + 1;
};
  $grid = array_merge($grid, [$line]);
  $y = $y + 1;
}
function randInt($s, $n) {
  global $width, $height, $iterations, $grid, $y, $line, $x, $seed, $vertices, $px, $py, $i, $r, $idx, $v;
  $next = ($s * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
}
$seed = 1;
$vertices = [[0, $height - 1], [$width - 1, $height - 1], [intval((intdiv($width, 2))), 0]];
$px = intval((intdiv($width, 2)));
$py = intval((intdiv($height, 2)));
$i = 0;
while ($i < $iterations) {
  $r = randInt($seed, 3);
  $seed = $r[0];
  $idx = ord($r[1]);
  $v = $vertices[$idx];
  $px = intval((($px + $v[0]) / 2));
  $py = intval((($py + $v[1]) / 2));
  if ($px >= 0 && $px < $width && $py >= 0 && $py < $height) {
  $grid[$py][$px] = '*';
}
  $i = $i + 1;
}
$y = 0;
while ($y < $height) {
  $line = '';
  $x = 0;
  while ($x < $width) {
  $line = $line . $grid[$y][$x];
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
}
