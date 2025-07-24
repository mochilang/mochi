<?php
ini_set('memory_limit', '-1');
$grid = [['.', '.', '.', '.', '.'], ['.', '#', '#', '#', '.'], ['.', '#', '.', '#', '.'], ['.', '#', '#', '#', '.'], ['.', '.', '.', '.', '.']];
function flood($x, $y, $repl) {
  global $grid, $line;
  $target = $grid[$y][$x];
  if ($target == $repl) {
  return;
}
  $ff = function($px, $py) use (&$ff, $x, $y, $repl, $target, &$grid) {
  if ($px < 0 || $py < 0 || $py >= count($grid) || $px >= count($grid[0])) {
  return;
}
  if ($grid[$py][$px] != $target) {
  return;
}
  $grid[$py][$px] = $repl;
  $ff($px - 1, $py);
  $ff($px + 1, $py);
  $ff($px, $py - 1);
  $ff($px, $py + 1);
};
  $ff($x, $y);
}
flood(2, 2, 'o');
foreach ($grid as $row) {
  $line = '';
  foreach ($row as $ch) {
  $line = $line . $ch;
};
  echo rtrim($line), PHP_EOL;
}
