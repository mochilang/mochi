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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  $G = 0.000000000066743;
  $C = 299792458.0;
  $PI = 3.141592653589793;
  function pow10($n) {
  global $C, $G, $PI;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function mochi_sqrt($x) {
  global $C, $G, $PI;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function mochi_abs($x) {
  global $C, $G, $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function capture_radii($target_body_radius, $target_body_mass, $projectile_velocity) {
  global $C, $G, $PI;
  if ($target_body_mass < 0.0) {
  _panic('Mass cannot be less than 0');
}
  if ($target_body_radius < 0.0) {
  _panic('Radius cannot be less than 0');
}
  if ($projectile_velocity > $C) {
  _panic('Cannot go beyond speed of light');
}
  $escape_velocity_squared = (2.0 * $G * $target_body_mass) / $target_body_radius;
  $denom = $projectile_velocity * $projectile_velocity;
  $capture_radius = $target_body_radius * mochi_sqrt(1.0 + $escape_velocity_squared / $denom);
  return $capture_radius;
};
  function capture_area($capture_radius) {
  global $C, $G, $PI;
  if ($capture_radius < 0.0) {
  _panic('Cannot have a capture radius less than 0');
}
  $sigma = $PI * $capture_radius * $capture_radius;
  return $sigma;
};
  function run_tests() {
  global $C, $G, $PI;
  $r = capture_radii(695699999.999999985078603, 1989999999999999991118215802998.747676610946655, 25000.0);
  if (mochi_abs($r - 17209590691.437139930997091) > 1.0) {
  _panic('capture_radii failed');
}
  $a = capture_area($r);
  if (mochi_abs($a - 930445533180181172383.527155034244061) > 1.0) {
  _panic('capture_area failed');
}
};
  function main() {
  global $C, $G, $PI;
  run_tests();
  $r = capture_radii(695699999.999999985078603, 1989999999999999991118215802998.747676610946655, 25000.0);
  echo rtrim(_str($r)), PHP_EOL;
  echo rtrim(_str(capture_area($r))), PHP_EOL;
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
