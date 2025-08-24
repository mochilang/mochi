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
  function equilibrium_index($arr) {
  global $arr1, $arr2, $arr3, $arr4;
  $total = 0;
  $i = 0;
  while ($i < count($arr)) {
  $total = $total + $arr[$i];
  $i = $i + 1;
};
  $left = 0;
  $i = 0;
  while ($i < count($arr)) {
  $total = $total - $arr[$i];
  if ($left == $total) {
  return $i;
}
  $left = $left + $arr[$i];
  $i = $i + 1;
};
  return -1;
};
  $arr1 = [-7, 1, 5, 2, -4, 3, 0];
  echo rtrim(json_encode(equilibrium_index($arr1), 1344)), PHP_EOL;
  $arr2 = [1, 2, 3, 4, 5];
  echo rtrim(json_encode(equilibrium_index($arr2), 1344)), PHP_EOL;
  $arr3 = [1, 1, 1, 1, 1];
  echo rtrim(json_encode(equilibrium_index($arr3), 1344)), PHP_EOL;
  $arr4 = [2, 4, 6, 8, 10, 3];
  echo rtrim(json_encode(equilibrium_index($arr4), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
