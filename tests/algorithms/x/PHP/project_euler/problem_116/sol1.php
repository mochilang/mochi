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
  function solution($length) {
  $ways = [];
  $i = 0;
  while ($i <= $length) {
  $row = [];
  $row = _append($row, 0);
  $row = _append($row, 0);
  $row = _append($row, 0);
  $ways = _append($ways, $row);
  $i = $i + 1;
};
  $row_length = 0;
  while ($row_length <= $length) {
  $tile_length = 2;
  while ($tile_length <= 4) {
  $tile_start = 0;
  while ($tile_start <= $row_length - $tile_length) {
  $remaining = $row_length - $tile_start - $tile_length;
  $ways[$row_length][$tile_length - 2] = $ways[$row_length][$tile_length - 2] + $ways[$remaining][$tile_length - 2] + 1;
  $tile_start = $tile_start + 1;
};
  $tile_length = $tile_length + 1;
};
  $row_length = $row_length + 1;
};
  $total = 0;
  $j = 0;
  while ($j < 3) {
  $total = $total + $ways[$length][$j];
  $j = $j + 1;
};
  return $total;
};
  echo rtrim(json_encode(solution(5), 1344)), PHP_EOL;
  echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
