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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $PI = 3.141592653589793;
  $TWO_PI = 6.283185307179586;
  $g = 9.80665;
  function _mod($x, $m) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  return $x - (floatval(intval($x / $m))) * $m;
};
  function sin($x) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  $y = _mod($x + $PI, $TWO_PI) - $PI;
  $y2 = $y * $y;
  $y3 = $y2 * $y;
  $y5 = $y3 * $y2;
  $y7 = $y5 * $y2;
  return $y - $y3 / 6.0 + $y5 / 120.0 - $y7 / 5040.0;
};
  function deg_to_rad($deg) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  return $deg * $PI / 180.0;
};
  function mochi_floor($x) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function mochi_round($x, $n) {
  global $PI, $TWO_PI, $g, $v0, $angle;
  $m = pow10($n);
  $y = mochi_floor($x * $m + 0.5);
  return $y / $m;
};
  function check_args($init_velocity, $angle) {
  global $PI, $TWO_PI, $g, $v0;
  if ($angle > 90.0 || $angle < 1.0) {
  _panic('Invalid angle. Range is 1-90 degrees.');
}
  if ($init_velocity < 0.0) {
  _panic('Invalid velocity. Should be a positive number.');
}
};
  function horizontal_distance($init_velocity, $angle) {
  global $PI, $TWO_PI, $g, $v0;
  check_args($init_velocity, $angle);
  $radians = deg_to_rad(2.0 * $angle);
  return mochi_round(($init_velocity * $init_velocity * sin($radians)) / $g, 2);
};
  function max_height($init_velocity, $angle) {
  global $PI, $TWO_PI, $g, $v0;
  check_args($init_velocity, $angle);
  $radians = deg_to_rad($angle);
  $s = sin($radians);
  return mochi_round(($init_velocity * $init_velocity * $s * $s) / (2.0 * $g), 2);
};
  function total_time($init_velocity, $angle) {
  global $PI, $TWO_PI, $g, $v0;
  check_args($init_velocity, $angle);
  $radians = deg_to_rad($angle);
  return mochi_round((2.0 * $init_velocity * sin($radians)) / $g, 2);
};
  $v0 = 25.0;
  $angle = 20.0;
  echo rtrim(json_encode(horizontal_distance($v0, $angle), 1344)), PHP_EOL;
  echo rtrim(json_encode(max_height($v0, $angle), 1344)), PHP_EOL;
  echo rtrim(json_encode(total_time($v0, $angle), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
