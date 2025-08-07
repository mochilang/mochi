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
$__start_mem = memory_get_usage();
$__start = _now();
  function max_sum_sliding_window($arr, $k) {
  if ($k < 0 || count($arr) < $k) {
  $panic('Invalid Input');
}
  $idx = 0;
  $current_sum = 0;
  while ($idx < $k) {
  $current_sum = $current_sum + $arr[$idx];
  $idx = $idx + 1;
};
  $max_sum = $current_sum;
  $i = 0;
  while ($i < count($arr) - $k) {
  $current_sum = $current_sum - $arr[$i] + $arr[$i + $k];
  if ($current_sum > $max_sum) {
  $max_sum = $current_sum;
}
  $i = $i + 1;
};
  return $max_sum;
};
  function test_max_sum_sliding_window() {
  $arr1 = [1, 4, 2, 10, 2, 3, 1, 0, 20];
  if (max_sum_sliding_window($arr1, 4) != 24) {
  $panic('test1 failed');
}
  $arr2 = [1, 4, 2, 10, 2, 13, 1, 0, 2];
  if (max_sum_sliding_window($arr2, 4) != 27) {
  $panic('test2 failed');
}
};
  function main() {
  test_max_sum_sliding_window();
  $sample = [1, 4, 2, 10, 2, 3, 1, 0, 20];
  echo rtrim(_str(max_sum_sliding_window($sample, 4))), PHP_EOL;
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
