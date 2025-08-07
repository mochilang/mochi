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
  function mochi_floor($x) {
  global $PI;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function mochi_pow($x, $n) {
  global $PI;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * $x;
  $i = $i + 1;
};
  return $result;
};
  function factorial($n) {
  global $PI;
  $result = 1.0;
  $i = 2;
  while ($i <= $n) {
  $result = $result * (floatval($i));
  $i = $i + 1;
};
  return $result;
};
  function radians($deg) {
  global $PI;
  return $deg * $PI / 180.0;
};
  function taylor_sin($angle_in_degrees, $accuracy, $rounded_values_count) {
  global $PI;
  $k = mochi_floor($angle_in_degrees / 360.0);
  $angle = $angle_in_degrees - ($k * 360.0);
  $angle_in_radians = radians($angle);
  $result = $angle_in_radians;
  $a = 3;
  $sign = -1.0;
  $i = 0;
  while ($i < $accuracy) {
  $result = $result + ($sign * mochi_pow($angle_in_radians, $a)) / factorial($a);
  $sign = -$sign;
  $a = $a + 2;
  $i = $i + 1;
};
  return $result;
};
  function test_sin() {
  global $PI;
  $eps = 0.0000001;
  if (mochi_abs(taylor_sin(0.0, 18, 10) - 0.0) > $eps) {
  $panic('sin(0) failed');
}
  if (mochi_abs(taylor_sin(90.0, 18, 10) - 1.0) > $eps) {
  $panic('sin(90) failed');
}
  if (mochi_abs(taylor_sin(180.0, 18, 10) - 0.0) > $eps) {
  $panic('sin(180) failed');
}
  if (mochi_abs(taylor_sin(270.0, 18, 10) - (-1.0)) > $eps) {
  $panic('sin(270) failed');
}
};
  function main() {
  global $PI;
  test_sin();
  $res = taylor_sin(64.0, 18, 10);
  echo rtrim(json_encode($res, 1344)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
