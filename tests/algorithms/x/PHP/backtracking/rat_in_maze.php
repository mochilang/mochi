<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function run_maze($maze, $i, $j, $dr, $dc, &$sol) {
  global $n;
  $size = count($maze);
  if ($i == $dr && $j == $dc && $maze[$i][$j] == 0) {
  $sol[$i][$j] = 0;
  return true;
}
  $lower_flag = ($i >= 0) && ($j >= 0);
  $upper_flag = ($i < $size) && ($j < $size);
  if ($lower_flag && $upper_flag) {
  $block_flag = ($sol[$i][$j] == 1) && ($maze[$i][$j] == 0);
  if ($block_flag) {
  $sol[$i][$j] = 0;
  if (run_maze($maze, $i + 1, $j, $dr, $dc, $sol) || run_maze($maze, $i, $j + 1, $dr, $dc, $sol) || run_maze($maze, $i - 1, $j, $dr, $dc, $sol) || run_maze($maze, $i, $j - 1, $dr, $dc, $sol)) {
  return true;
};
  $sol[$i][$j] = 1;
  return false;
};
}
  return false;
}
function solve_maze($maze, $sr, $sc, $dr, $dc) {
  global $n;
  $size = count($maze);
  if (!(0 <= $sr && $sr < $size && 0 <= $sc && $sc < $size && 0 <= $dr && $dr < $size && 0 <= $dc && $dc < $size)) {
  $panic('Invalid source or destination coordinates');
}
  $sol = [];
  $i = 0;
  while ($i < $size) {
  $row = [];
  $j = 0;
  while ($j < $size) {
  $row = _append($row, 1);
  $j = $j + 1;
};
  $sol = _append($sol, $row);
  $i = $i + 1;
};
  $solved = run_maze($maze, $sr, $sc, $dr, $dc, $sol);
  if ($solved) {
  return $sol;
} else {
  $panic('No solution exists!');
}
}
$maze = [[0, 1, 0, 1, 1], [0, 0, 0, 0, 0], [1, 0, 1, 0, 1], [0, 0, 1, 0, 0], [1, 0, 0, 1, 0]];
$n = count($maze) - 1;
echo rtrim(_str(solve_maze($maze, 0, 0, $n, $n))), PHP_EOL;
