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
  function find_min_iterative($nums) {
  if (count($nums) == 0) {
  $panic('find_min_iterative() arg is an empty sequence');
}
  $min_num = $nums[0];
  $i = 0;
  while ($i < count($nums)) {
  $num = $nums[$i];
  if ($num < $min_num) {
  $min_num = $num;
}
  $i = $i + 1;
};
  return $min_num;
};
  function find_min_recursive($nums, $left, $right) {
  $n = count($nums);
  if ($n == 0) {
  $panic('find_min_recursive() arg is an empty sequence');
}
  if ($left >= $n || $left < (0 - $n) || $right >= $n || $right < (0 - $n)) {
  $panic('list index out of range');
}
  $l = $left;
  $r = $right;
  if ($l < 0) {
  $l = $n + $l;
}
  if ($r < 0) {
  $r = $n + $r;
}
  if ($l == $r) {
  return $nums[$l];
}
  $mid = _intdiv(($l + $r), 2);
  $left_min = find_min_recursive($nums, $l, $mid);
  $right_min = find_min_recursive($nums, $mid + 1, $r);
  if ($left_min <= $right_min) {
  return $left_min;
}
  return $right_min;
};
  function test_find_min() {
  $a = [3.0, 2.0, 1.0];
  if (find_min_iterative($a) != 1.0) {
  $panic('iterative test1 failed');
}
  if (find_min_recursive($a, 0, count($a) - 1) != 1.0) {
  $panic('recursive test1 failed');
}
  $b = [-3.0, -2.0, -1.0];
  if (find_min_iterative($b) != (-3.0)) {
  $panic('iterative test2 failed');
}
  if (find_min_recursive($b, 0, count($b) - 1) != (-3.0)) {
  $panic('recursive test2 failed');
}
  $c = [3.0, -3.0, 0.0];
  if (find_min_iterative($c) != (-3.0)) {
  $panic('iterative test3 failed');
}
  if (find_min_recursive($c, 0, count($c) - 1) != (-3.0)) {
  $panic('recursive test3 failed');
}
  $d = [1.0, 3.0, 5.0, 7.0, 9.0, 2.0, 4.0, 6.0, 8.0, 10.0];
  if (find_min_recursive($d, (0 - count($d)), (0 - 1)) != 1.0) {
  $panic('negative index test failed');
}
};
  function main() {
  test_find_min();
  $sample = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, -3.0, 24.0, -56.0];
  echo rtrim(_str(find_min_iterative($sample))), PHP_EOL;
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
