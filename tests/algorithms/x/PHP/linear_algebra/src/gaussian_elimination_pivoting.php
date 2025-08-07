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
$__start_mem = memory_get_usage();
$__start = _now();
  function panic($msg) {
  global $example_matrix, $solution;
  echo rtrim($msg), PHP_EOL;
};
  function abs_float($x) {
  global $example_matrix, $solution;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function copy_matrix($src) {
  global $example_matrix, $solution;
  $res = [];
  $i = 0;
  while ($i < count($src)) {
  $row_src = $src[$i];
  $row = [];
  $j = 0;
  while ($j < count($row_src)) {
  $row = _append($row, $row_src[$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function solve_linear_system($matrix) {
  global $example_matrix, $solution;
  $ab = copy_matrix($matrix);
  $num_rows = count($ab);
  $num_cols = count($ab[0]) - 1;
  if ($num_rows != $num_cols) {
  panic('Matrix is not square');
  return [];
}
  $column_num = 0;
  while ($column_num < $num_rows) {
  $i = $column_num;
  while ($i < $num_cols) {
  if (abs_float($ab[$i][$column_num]) > abs_float($ab[$column_num][$column_num])) {
  $temp = $ab[$column_num];
  $ab[$column_num] = $ab[$i];
  $ab[$i] = $temp;
}
  $i = $i + 1;
};
  if (abs_float($ab[$column_num][$column_num]) < 0.00000001) {
  panic('Matrix is singular');
  return [];
}
  if ($column_num != 0) {
  $i = $column_num;
  while ($i < $num_rows) {
  $factor = $ab[$i][$column_num - 1] / $ab[$column_num - 1][$column_num - 1];
  $j = 0;
  while ($j < count($ab[$i])) {
  $ab[$i][$j] = $ab[$i][$j] - $factor * $ab[$column_num - 1][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
}
  $column_num = $column_num + 1;
};
  $x_lst = [];
  $t = 0;
  while ($t < $num_rows) {
  $x_lst = _append($x_lst, 0.0);
  $t = $t + 1;
};
  $column_num = $num_rows - 1;
  while ($column_num >= 0) {
  $x = $ab[$column_num][$num_cols] / $ab[$column_num][$column_num];
  $x_lst[$column_num] = $x;
  $i = $column_num - 1;
  while ($i >= 0) {
  $ab[$i][$num_cols] = $ab[$i][$num_cols] - $ab[$i][$column_num] * $x;
  $i = $i - 1;
};
  $column_num = $column_num - 1;
};
  return $x_lst;
};
  $example_matrix = [[5.0, -5.0, -3.0, 4.0, -11.0], [1.0, -4.0, 6.0, -4.0, -10.0], [-2.0, -5.0, 4.0, -5.0, -12.0], [-3.0, -3.0, 5.0, -5.0, 8.0]];
  echo rtrim('Matrix:'), PHP_EOL;
  echo rtrim(_str($example_matrix)), PHP_EOL;
  $solution = solve_linear_system($example_matrix);
  echo rtrim(_str($solution)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
