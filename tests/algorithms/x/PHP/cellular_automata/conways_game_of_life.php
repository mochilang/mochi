<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$GLIDER = [[0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 0], [1, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]];
$BLINKER = [[0, 1, 0], [0, 1, 0], [0, 1, 0]];
function new_generation($cells) {
  global $GLIDER, $BLINKER;
  $rows = count($cells);
  $cols = count($cells[0]);
  $next = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $count = 0;
  if ($i > 0 && $j > 0) {
  $count = $count + $cells[$i - 1][$j - 1];
}
  if ($i > 0) {
  $count = $count + $cells[$i - 1][$j];
}
  if ($i > 0 && $j < $cols - 1) {
  $count = $count + $cells[$i - 1][$j + 1];
}
  if ($j > 0) {
  $count = $count + $cells[$i][$j - 1];
}
  if ($j < $cols - 1) {
  $count = $count + $cells[$i][$j + 1];
}
  if ($i < $rows - 1 && $j > 0) {
  $count = $count + $cells[$i + 1][$j - 1];
}
  if ($i < $rows - 1) {
  $count = $count + $cells[$i + 1][$j];
}
  if ($i < $rows - 1 && $j < $cols - 1) {
  $count = $count + $cells[$i + 1][$j + 1];
}
  $alive = $cells[$i][$j] == 1;
  if (($alive && $count >= 2 && $count <= 3) || (!$alive && $count == 3)) {
  $row = _append($row, 1);
} else {
  $row = _append($row, 0);
}
  $j = $j + 1;
};
  $next = _append($next, $row);
  $i = $i + 1;
};
  return $next;
}
function generate_generations($cells, $frames) {
  global $GLIDER, $BLINKER;
  $result = [];
  $i = 0;
  $current = $cells;
  while ($i < $frames) {
  $result = _append($result, $current);
  $current = new_generation($current);
  $i = $i + 1;
};
  return $result;
}
function main() {
  global $GLIDER, $BLINKER;
  $frames = generate_generations($GLIDER, 4);
  $i = 0;
  while ($i < count($frames)) {
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($frames[$i], 1344))))))), PHP_EOL;
  $i = $i + 1;
};
}
main();
