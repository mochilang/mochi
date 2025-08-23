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
  function search($list_data, $key, $left, $right) {
  $r = $right;
  if ($r == 0) {
  $r = count($list_data) - 1;
}
  if ($left > $r) {
  return -1;
} else {
  if ($list_data[$left] == $key) {
  return $left;
} else {
  if ($list_data[$r] == $key) {
  return $r;
} else {
  return search($list_data, $key, $left + 1, $r - 1);
};
};
}
};
  function main() {
  echo rtrim(json_encode(search([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 5, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([1, 2, 4, 5, 3], 4, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([1, 2, 4, 5, 3], 6, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([5], 5, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([], 1, 0, 0), 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
