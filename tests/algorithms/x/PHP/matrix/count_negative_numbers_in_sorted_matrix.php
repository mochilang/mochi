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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function generate_large_matrix() {
  global $grid, $test_grids, $results_bin, $results_brute, $results_break;
  $result = [];
  $i = 0;
  while ($i < 1000) {
  $row = [];
  $j = 1000 - $i;
  while ($j > (-1000 - $i)) {
  $row = _append($row, $j);
  $j = $j - 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function find_negative_index($arr) {
  global $grid, $test_grids, $results_bin, $i, $results_brute, $results_break;
  $left = 0;
  $right = count($arr) - 1;
  if (count($arr) == 0) {
  return 0;
}
  if ($arr[0] < 0) {
  return 0;
}
  while ($left <= $right) {
  $mid = _intdiv(($left + $right), 2);
  $num = $arr[$mid];
  if ($num < 0) {
  if ($mid == 0) {
  return 0;
};
  if ($arr[$mid - 1] >= 0) {
  return $mid;
};
  $right = $mid - 1;
} else {
  $left = $mid + 1;
}
};
  return count($arr);
};
  function count_negatives_binary_search($grid) {
  global $test_grids, $results_bin, $results_brute, $results_break;
  $total = 0;
  $bound = count($grid[0]);
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $idx = find_negative_index(array_slice($row, 0, $bound - 0));
  $bound = $idx;
  $total = $total + $idx;
  $i = $i + 1;
};
  return (count($grid) * count($grid[0])) - $total;
};
  function count_negatives_brute_force($grid) {
  global $test_grids, $results_bin, $results_brute, $results_break;
  $count = 0;
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $j = 0;
  while ($j < count($row)) {
  if ($row[$j] < 0) {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $count;
};
  function count_negatives_brute_force_with_break($grid) {
  global $test_grids, $results_bin, $results_brute, $results_break;
  $total = 0;
  $i = 0;
  while ($i < count($grid)) {
  $row = $grid[$i];
  $j = 0;
  while ($j < count($row)) {
  $number = $row[$j];
  if ($number < 0) {
  $total = $total + (count($row) - $j);
  break;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $total;
};
  $grid = generate_large_matrix();
  $test_grids = [[[4, 3, 2, -1], [3, 2, 1, -1], [1, 1, -1, -2], [-1, -1, -2, -3]], [[3, 2], [1, 0]], [[7, 7, 6]], [[7, 7, 6], [-1, -2, -3]], $grid];
  $results_bin = [];
  $i = 0;
  while ($i < count($test_grids)) {
  $results_bin = _append($results_bin, count_negatives_binary_search($test_grids[$i]));
  $i = $i + 1;
}
  echo rtrim(_str($results_bin)), PHP_EOL;
  $results_brute = [];
  $i = 0;
  while ($i < count($test_grids)) {
  $results_brute = _append($results_brute, count_negatives_brute_force($test_grids[$i]));
  $i = $i + 1;
}
  echo rtrim(_str($results_brute)), PHP_EOL;
  $results_break = [];
  $i = 0;
  while ($i < count($test_grids)) {
  $results_break = _append($results_break, count_negatives_brute_force_with_break($test_grids[$i]));
  $i = $i + 1;
}
  echo rtrim(_str($results_break)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
