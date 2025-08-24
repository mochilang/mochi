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
$__start_mem = memory_get_usage();
$__start = _now();
  function partition(&$arr, $low, $high) {
  global $arr1, $arr2;
  $pivot = $arr[$high];
  $i = $low - 1;
  $j = $low;
  while ($j < $high) {
  if ($arr[$j] >= $pivot) {
  $i = $i + 1;
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = $j + 1;
};
  $k = $i + 1;
  $tmp = $arr[$k];
  $arr[$k] = $arr[$high];
  $arr[$high] = $tmp;
  return $k;
};
  function &kth_largest_element(&$arr, $position) {
  global $arr1, $arr2;
  if (count($arr) == 0) {
  return -1;
}
  if ($position < 1 || $position > count($arr)) {
  return -1;
}
  $low = 0;
  $high = count($arr) - 1;
  while ($low <= $high) {
  if ($low > count($arr) - 1 || $high < 0) {
  return -1;
}
  $pivot_index = partition($arr, $low, $high);
  if ($pivot_index == $position - 1) {
  return $arr[$pivot_index];
} else {
  if ($pivot_index > $position - 1) {
  $high = $pivot_index - 1;
} else {
  $low = $pivot_index + 1;
};
}
};
  return -1;
};
  $arr1 = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5];
  echo rtrim(json_encode(kth_largest_element($arr1, 3), 1344)), PHP_EOL;
  echo rtrim('
'), PHP_EOL;
  $arr2 = [2, 5, 6, 1, 9, 3, 8, 4, 7, 3, 5];
  echo rtrim(json_encode(kth_largest_element($arr2, 1), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
