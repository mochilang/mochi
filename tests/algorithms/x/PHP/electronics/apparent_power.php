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
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  function mochi_abs($x) {
  global $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function to_radians($deg) {
  global $PI;
  return $deg * $PI / 180.0;
};
  function sin_taylor($x) {
  global $PI;
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = $k1 + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function cos_taylor($x) {
  global $PI;
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function rect($mag, $angle) {
  global $PI;
  $c = cos_taylor($angle);
  $s = sin_taylor($angle);
  return [$mag * $c, $mag * $s];
};
  function multiply($a, $b) {
  global $PI;
  return [$a[0] * $b[0] - $a[1] * $b[1], $a[0] * $b[1] + $a[1] * $b[0]];
};
  function apparent_power($voltage, $current, $voltage_angle, $current_angle) {
  global $PI;
  $vrad = to_radians($voltage_angle);
  $irad = to_radians($current_angle);
  $vrect = rect($voltage, $vrad);
  $irect = rect($current, $irad);
  $result = multiply($vrect, $irect);
  return $result;
};
  function approx_equal($a, $b, $eps) {
  global $PI;
  return mochi_abs($a[0] - $b[0]) < $eps && mochi_abs($a[1] - $b[1]) < $eps;
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
