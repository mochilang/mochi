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
  function pow10($n) {
  $p = 1.0;
  $k = 0;
  if ($n >= 0) {
  while ($k < $n) {
  $p = $p * 10.0;
  $k = $k + 1;
};
} else {
  $m = -$n;
  while ($k < $m) {
  $p = $p / 10.0;
  $k = $k + 1;
};
}
  return $p;
};
  function sqrt_newton($n) {
  if ($n == 0.0) {
  return 0.0;
}
  $x = $n;
  $j = 0;
  while ($j < 20) {
  $x = ($x + $n / $x) / 2.0;
  $j = $j + 1;
};
  return $x;
};
  function round3($x) {
  $y = $x * 1000.0 + 0.5;
  $yi = intval($y);
  if ((floatval($yi)) > $y) {
  $yi = $yi - 1;
}
  return (floatval($yi)) / 1000.0;
};
  function escape_velocity($mass, $radius) {
  if ($radius == 0.0) {
  _panic('Radius cannot be zero.');
}
  $G = 6.6743 * pow10(-11);
  $velocity = sqrt_newton(2.0 * $G * $mass / $radius);
  return round3($velocity);
};
  echo rtrim(json_encode(escape_velocity(5.972 * pow10(24), 6.371 * pow10(6)), 1344)), PHP_EOL;
  echo rtrim(json_encode(escape_velocity(7.348 * pow10(22), 1.737 * pow10(6)), 1344)), PHP_EOL;
  echo rtrim(json_encode(escape_velocity(1.898 * pow10(27), 6.9911 * pow10(7)), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
