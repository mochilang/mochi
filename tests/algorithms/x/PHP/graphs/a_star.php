<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$DIRECTIONS = [[-1, 0], [0, -1], [1, 0], [0, 1]];
function iabs($x) {
  global $DIRECTIONS;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
function search($grid, $init, $goal, $cost, $heuristic) {
  global $DIRECTIONS;
  $closed = [];
  $r = 0;
  while ($r < count($grid)) {
  $row = [];
  $c = 0;
  while ($c < count($grid[0])) {
  $row = _append($row, 0);
  $c = $c + 1;
};
  $closed = _append($closed, $row);
  $r = $r + 1;
};
  $closed[$init[0]][$init[1]] = 1;
  $action = [];
  $r = 0;
  while ($r < count($grid)) {
  $row = [];
  $c = 0;
  while ($c < count($grid[0])) {
  $row = _append($row, 0);
  $c = $c + 1;
};
  $action = _append($action, $row);
  $r = $r + 1;
};
  $x = $init[0];
  $y = $init[1];
  $g = 0;
  $f = $g + $heuristic[$x][$y];
  $cell = [[$f, $g, $x, $y]];
  $found = false;
  $resign = false;
  while ((!$found) && (!$resign)) {
  if (count($cell) == 0) {
  $panic('Algorithm is unable to find solution');
} else {
  $best_i = 0;
  $best_f = $cell[0][0];
  $i = 1;
  while ($i < count($cell)) {
  if ($cell[$i][0] < $best_f) {
  $best_f = $cell[$i][0];
  $best_i = $i;
}
  $i = $i + 1;
};
  $next_cell = $cell[$best_i];
  $new_cell = [];
  $i = 0;
  while ($i < count($cell)) {
  if ($i != $best_i) {
  $new_cell = _append($new_cell, $cell[$i]);
}
  $i = $i + 1;
};
  $cell = $new_cell;
  $x = $next_cell[2];
  $y = $next_cell[3];
  $g = $next_cell[1];
  if ($x == $goal[0] && $y == $goal[1]) {
  $found = true;
} else {
  $d = 0;
  while ($d < count($DIRECTIONS)) {
  $x2 = $x + $DIRECTIONS[$d][0];
  $y2 = $y + $DIRECTIONS[$d][1];
  if ($x2 >= 0 && $x2 < count($grid) && $y2 >= 0 && $y2 < count($grid[0]) && $closed[$x2][$y2] == 0 && $grid[$x2][$y2] == 0) {
  $g2 = $g + $cost;
  $f2 = $g2 + $heuristic[$x2][$y2];
  $cell = _append($cell, [$f2, $g2, $x2, $y2]);
  $closed[$x2][$y2] = 1;
  $action[$x2][$y2] = $d;
}
  $d = $d + 1;
};
};
}
};
  $invpath = [];
  $x = $goal[0];
  $y = $goal[1];
  $invpath = _append($invpath, [$x, $y]);
  while ($x != $init[0] || $y != $init[1]) {
  $dir = $action[$x][$y];
  $x2 = $x - $DIRECTIONS[$dir][0];
  $y2 = $y - $DIRECTIONS[$dir][1];
  $x = $x2;
  $y = $y2;
  $invpath = _append($invpath, [$x, $y]);
};
  $path = [];
  $idx = count($invpath) - 1;
  while ($idx >= 0) {
  $path = _append($path, $invpath[$idx]);
  $idx = $idx - 1;
};
  return ['path' => $path, 'action' => $action];
}
function main() {
  global $DIRECTIONS;
  $grid = [[0, 1, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0], [0, 1, 0, 0, 1, 0], [0, 0, 0, 0, 1, 0]];
  $init = [0, 0];
  $goal = [count($grid) - 1, count($grid[0]) - 1];
  $cost = 1;
  $heuristic = [];
  $i = 0;
  while ($i < count($grid)) {
  $row = [];
  $j = 0;
  while ($j < count($grid[0])) {
  $h = iabs($i - $goal[0]) + iabs($j - $goal[1]);
  if ($grid[$i][$j] == 1) {
  $row = _append($row, 99);
} else {
  $row = _append($row, $h);
}
  $j = $j + 1;
};
  $heuristic = _append($heuristic, $row);
  $i = $i + 1;
};
  $result = search($grid, $init, $goal, $cost, $heuristic);
  echo rtrim('ACTION MAP'), PHP_EOL;
  $rr = 0;
  while ($rr < _len($result['action'])) {
  echo rtrim(json_encode($result['action'][$rr], 1344)), PHP_EOL;
  $rr = $rr + 1;
};
  $p = 0;
  while ($p < _len($result['path'])) {
  echo rtrim(json_encode($result['path'][$p], 1344)), PHP_EOL;
  $p = $p + 1;
};
}
main();
