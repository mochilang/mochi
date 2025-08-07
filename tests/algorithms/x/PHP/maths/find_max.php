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
  function normalize_index($index, $n) {
  if ($index < 0) {
  return $n + $index;
}
  return $index;
};
  function find_max_iterative($nums) {
  if (count($nums) == 0) {
  $panic('find_max_iterative() arg is an empty sequence');
}
  $max_num = $nums[0];
  $i = 0;
  while ($i < count($nums)) {
  $x = $nums[$i];
  if ($x > $max_num) {
  $max_num = $x;
}
  $i = $i + 1;
};
  return $max_num;
};
  function find_max_recursive($nums, $left, $right) {
  $n = count($nums);
  if ($n == 0) {
  $panic('find_max_recursive() arg is an empty sequence');
}
  if ($left >= $n || $left < (0 - $n) || $right >= $n || $right < (0 - $n)) {
  $panic('list index out of range');
}
  $l = normalize_index($left, $n);
  $r = normalize_index($right, $n);
  if ($l == $r) {
  return $nums[$l];
}
  $mid = _intdiv(($l + $r), 2);
  $left_max = find_max_recursive($nums, $l, $mid);
  $right_max = find_max_recursive($nums, $mid + 1, $r);
  if ($left_max >= $right_max) {
  return $left_max;
}
  return $right_max;
};
  function test_find_max() {
  $arr = [2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0];
  if (find_max_iterative($arr) != 94.0) {
  $panic('find_max_iterative failed');
}
  if (find_max_recursive($arr, 0, count($arr) - 1) != 94.0) {
  $panic('find_max_recursive failed');
}
  if (find_max_recursive($arr, -count($arr), -1) != 94.0) {
  $panic('negative index handling failed');
}
};
  function main() {
  test_find_max();
  $nums = [2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0];
  echo rtrim(json_encode(find_max_iterative($nums), 1344)), PHP_EOL;
  echo rtrim(json_encode(find_max_recursive($nums, 0, count($nums) - 1), 1344)), PHP_EOL;
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
