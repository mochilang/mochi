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
  function pairs_with_sum($arr, $req_sum) {
  $n = count($arr);
  $count = 0;
  $i = 0;
  while ($i < $n) {
  $j = $i + 1;
  while ($j < $n) {
  if ($arr[$i] + $arr[$j] == $req_sum) {
  $count = $count + 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $count;
};
  echo rtrim(json_encode(pairs_with_sum([1, 5, 7, 1], 6), 1344)), PHP_EOL;
  echo rtrim(json_encode(pairs_with_sum([1, 1, 1, 1, 1, 1, 1, 1], 2), 1344)), PHP_EOL;
  echo rtrim(json_encode(pairs_with_sum([1, 7, 6, 2, 5, 4, 3, 1, 9, 8], 7), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
