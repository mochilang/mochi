<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function binary_search($arr, $item) {
  global $arr1, $arr2, $arr3, $arr4, $arr5;
  $low = 0;
  $high = count($arr) - 1;
  while ($low <= $high) {
  $mid = _intdiv(($low + $high), 2);
  $val = $arr[$mid];
  if ($val == $item) {
  return true;
}
  if ($item < $val) {
  $high = $mid - 1;
} else {
  $low = $mid + 1;
}
};
  return false;
};
  $arr1 = [0, 1, 2, 8, 13, 17, 19, 32, 42];
  echo rtrim(json_encode(binary_search($arr1, 3), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_search($arr1, 13), 1344)), PHP_EOL;
  $arr2 = [4, 4, 5, 6, 7];
  echo rtrim(json_encode(binary_search($arr2, 4), 1344)), PHP_EOL;
  echo rtrim(json_encode(binary_search($arr2, -10), 1344)), PHP_EOL;
  $arr3 = [-18, 2];
  echo rtrim(json_encode(binary_search($arr3, -18), 1344)), PHP_EOL;
  $arr4 = [5];
  echo rtrim(json_encode(binary_search($arr4, 5), 1344)), PHP_EOL;
  $arr5 = [];
  echo rtrim(json_encode(binary_search($arr5, 1), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
