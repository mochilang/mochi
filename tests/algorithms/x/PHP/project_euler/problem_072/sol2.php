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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function solution($limit) {
  $phi = [];
  $i = 0;
  while ($i <= $limit) {
  $phi = _append($phi, $i);
  $i = $i + 1;
};
  $n = 2;
  while ($n <= $limit) {
  if ($phi[$n] == $n) {
  $k = $n;
  while ($k <= $limit) {
  $phi[$k] = $phi[$k] - $phi[$k] / $n;
  $k = $k + $n;
};
}
  $n = $n + 1;
};
  $total = 0;
  $m = 2;
  while ($m <= $limit) {
  $total = $total + $phi[$m];
  $m = $m + 1;
};
  return $total;
};
  function main() {
  echo rtrim(json_encode(solution(8), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(1000), 1344)), PHP_EOL;
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
