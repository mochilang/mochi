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
  function exp_taylor($x) {
  global $sample;
  $term = 1.0;
  $sum = 1.0;
  $i = 1.0;
  while ($i < 20.0) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1.0;
};
  return $sum;
};
  function sigmoid($vector) {
  global $sample;
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $value = 1.0 / (1.0 + exp_taylor(-$x));
  $result = _append($result, $value);
  $i = $i + 1;
};
  return $result;
};
  function gaussian_error_linear_unit($vector) {
  global $sample;
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $gelu = $x * (1.0 / (1.0 + exp_taylor(-1.702 * $x)));
  $result = _append($result, $gelu);
  $i = $i + 1;
};
  return $result;
};
  $sample = [-1.0, 1.0, 2.0];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(sigmoid($sample), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(gaussian_error_linear_unit($sample), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(gaussian_error_linear_unit([-3.0]), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
