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
  function is_harmonic_series($series) {
  if (count($series) == 0) {
  $panic('Input list must be a non empty list');
}
  if (count($series) == 1) {
  if ($series[0] == 0.0) {
  $panic('Input series cannot have 0 as an element');
};
  return true;
}
  $rec_series = [];
  $i = 0;
  while ($i < count($series)) {
  $val = $series[$i];
  if ($val == 0.0) {
  $panic('Input series cannot have 0 as an element');
}
  $rec_series = _append($rec_series, 1.0 / $val);
  $i = $i + 1;
};
  $common_diff = $rec_series[1] - $rec_series[0];
  $idx = 2;
  while ($idx < count($rec_series)) {
  if ($rec_series[$idx] - $rec_series[$idx - 1] != $common_diff) {
  return false;
}
  $idx = $idx + 1;
};
  return true;
};
  function harmonic_mean($series) {
  if (count($series) == 0) {
  $panic('Input list must be a non empty list');
}
  $total = 0.0;
  $i = 0;
  while ($i < count($series)) {
  $total = $total + 1.0 / $series[$i];
  $i = $i + 1;
};
  return (floatval(count($series))) / $total;
};
  echo rtrim(json_encode(is_harmonic_series([1.0, 2.0 / 3.0, 1.0 / 2.0, 2.0 / 5.0, 1.0 / 3.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_harmonic_series([1.0, 2.0 / 3.0, 2.0 / 5.0, 1.0 / 3.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(harmonic_mean([1.0, 4.0, 4.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(harmonic_mean([3.0, 6.0, 9.0, 12.0]), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
