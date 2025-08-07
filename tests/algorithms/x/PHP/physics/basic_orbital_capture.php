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
  global $G, $C, $PI;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
};
  function sqrt($x) {
  global $G, $C, $PI;
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
  global $G, $C, $PI;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function capture_radii($target_body_radius, $target_body_mass, $projectile_velocity) {
  global $G, $C, $PI;
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
  $capture_radius = $target_body_radius * sqrt(1.0 + $escape_velocity_squared / $denom);
  return $capture_radius;
};
  function capture_area($capture_radius) {
  global $G, $C, $PI;
  if ($capture_radius < 0.0) {
  _panic('Cannot have a capture radius less than 0');
}
  $sigma = $PI * $capture_radius * $capture_radius;
  return $sigma;
};
  function run_tests() {
  global $G, $C, $PI;
  $r = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0);
  if (mochi_abs($r - 1.720959069143714 * pow10(10)) > 1.0) {
  _panic('capture_radii failed');
}
  $a = capture_area($r);
  if (mochi_abs($a - 9.304455331801812 * pow10(20)) > 1.0) {
  _panic('capture_area failed');
}
};
  function main() {
  global $G, $C, $PI;
  run_tests();
  $r = capture_radii(6.957 * pow10(8), 1.99 * pow10(30), 25000.0);
  echo rtrim(_str($r)), PHP_EOL;
  echo rtrim(_str(capture_area($r))), PHP_EOL;
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
