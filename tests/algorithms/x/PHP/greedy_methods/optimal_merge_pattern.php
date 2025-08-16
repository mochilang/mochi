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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function index_of_min($xs) {
  $min_idx = 0;
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] < $xs[$min_idx]) {
  $min_idx = $i;
}
  $i = $i + 1;
};
  return $min_idx;
};
  function remove_at($xs, $idx) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function optimal_merge_pattern($files) {
  $arr = $files;
  $optimal_merge_cost = 0;
  while (count($arr) > 1) {
  $temp = 0;
  $k = 0;
  while ($k < 2) {
  $min_idx = index_of_min($arr);
  $temp = $temp + $arr[$min_idx];
  $arr = remove_at($arr, $min_idx);
  $k = $k + 1;
};
  $arr = _append($arr, $temp);
  $optimal_merge_cost = $optimal_merge_cost + $temp;
};
  return $optimal_merge_cost;
};
  echo rtrim(json_encode(optimal_merge_pattern([2, 3, 4]), 1344)), PHP_EOL;
  echo rtrim(json_encode(optimal_merge_pattern([5, 10, 20, 30, 30]), 1344)), PHP_EOL;
  echo rtrim(json_encode(optimal_merge_pattern([8, 8, 8, 8, 8]), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
