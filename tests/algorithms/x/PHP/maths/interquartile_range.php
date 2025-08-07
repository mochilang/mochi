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
  function bubble_sort($nums) {
  $arr = [];
  $i = 0;
  while ($i < count($nums)) {
  $arr = _append($arr, $nums[$i]);
  $i = $i + 1;
};
  $n = count($arr);
  $a = 0;
  while ($a < $n) {
  $b = 0;
  while ($b < $n - $a - 1) {
  if ($arr[$b] > $arr[$b + 1]) {
  $temp = $arr[$b];
  $arr[$b] = $arr[$b + 1];
  $arr[$b + 1] = $temp;
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  return $arr;
};
  function find_median($nums) {
  $length = count($nums);
  $div = _intdiv($length, 2);
  $mod = $length % 2;
  if ($mod != 0) {
  return $nums[$div];
}
  return ($nums[$div] + $nums[$div - 1]) / 2.0;
};
  function interquartile_range($nums) {
  if (count($nums) == 0) {
  $panic('The list is empty. Provide a non-empty list.');
}
  $sorted = bubble_sort($nums);
  $length = count($sorted);
  $div = _intdiv($length, 2);
  $mod = $length % 2;
  $lower = [];
  $i = 0;
  while ($i < $div) {
  $lower = _append($lower, $sorted[$i]);
  $i = $i + 1;
};
  $upper = [];
  $j = $div + $mod;
  while ($j < $length) {
  $upper = _append($upper, $sorted[$j]);
  $j = $j + 1;
};
  $q1 = find_median($lower);
  $q3 = find_median($upper);
  return $q3 - $q1;
};
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function float_equal($a, $b) {
  $diff = absf($a - $b);
  return $diff < 0.0000001;
};
  function test_interquartile_range() {
  if (!float_equal(interquartile_range([4.0, 1.0, 2.0, 3.0, 2.0]), 2.0)) {
  $panic('interquartile_range case1 failed');
}
  if (!float_equal(interquartile_range([-2.0, -7.0, -10.0, 9.0, 8.0, 4.0, -67.0, 45.0]), 17.0)) {
  $panic('interquartile_range case2 failed');
}
  if (!float_equal(interquartile_range([-2.1, -7.1, -10.1, 9.1, 8.1, 4.1, -67.1, 45.1]), 17.2)) {
  $panic('interquartile_range case3 failed');
}
  if (!float_equal(interquartile_range([0.0, 0.0, 0.0, 0.0, 0.0]), 0.0)) {
  $panic('interquartile_range case4 failed');
}
};
  function main() {
  test_interquartile_range();
  echo rtrim(_str(interquartile_range([4.0, 1.0, 2.0, 3.0, 2.0]))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
