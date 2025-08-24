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
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function ln($x) {
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return 2.0 * $sum;
};
  function mochi_log10($x) {
  return ln($x) / ln(10.0);
};
  function peak_signal_to_noise_ratio($original, $contrast) {
  $mse = 0.0;
  $i = 0;
  while ($i < count($original)) {
  $j = 0;
  while ($j < count($original[$i])) {
  $diff = floatval(($original[$i][$j] - $contrast[$i][$j]));
  $mse = $mse + $diff * $diff;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $size = floatval((count($original) * count($original[0])));
  $mse = $mse / $size;
  if ($mse == 0.0) {
  return 100.0;
}
  $PIXEL_MAX = 255.0;
  return 20.0 * mochi_log10($PIXEL_MAX / sqrtApprox($mse));
};
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
