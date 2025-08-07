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
  function runge_kutta_fehlberg_45($func, $x_initial, $y_initial, $step_size, $x_final) {
  if ($x_initial >= $x_final) {
  $panic('The final value of x must be greater than initial value of x.');
}
  if ($step_size <= 0.0) {
  $panic('Step size must be positive.');
}
  $n = intval((($x_final - $x_initial) / $step_size));
  $ys = [];
  $x = $x_initial;
  $y = $y_initial;
  $ys = _append($ys, $y);
  $i = 0;
  while ($i < $n) {
  $k1 = $step_size * $func($x, $y);
  $k2 = $step_size * $func($x + $step_size / 4.0, $y + $k1 / 4.0);
  $k3 = $step_size * $func($x + (3.0 / 8.0) * $step_size, $y + (3.0 / 32.0) * $k1 + (9.0 / 32.0) * $k2);
  $k4 = $step_size * $func($x + (12.0 / 13.0) * $step_size, $y + (1932.0 / 2197.0) * $k1 - (7200.0 / 2197.0) * $k2 + (7296.0 / 2197.0) * $k3);
  $k5 = $step_size * $func($x + $step_size, $y + (439.0 / 216.0) * $k1 - 8.0 * $k2 + (3680.0 / 513.0) * $k3 - (845.0 / 4104.0) * $k4);
  $k6 = $step_size * $func($x + $step_size / 2.0, $y - (8.0 / 27.0) * $k1 + 2.0 * $k2 - (3544.0 / 2565.0) * $k3 + (1859.0 / 4104.0) * $k4 - (11.0 / 40.0) * $k5);
  $y = $y + (16.0 / 135.0) * $k1 + (6656.0 / 12825.0) * $k3 + (28561.0 / 56430.0) * $k4 - (9.0 / 50.0) * $k5 + (2.0 / 55.0) * $k6;
  $x = $x + $step_size;
  $ys = _append($ys, $y);
  $i = $i + 1;
};
  return $ys;
};
  function main() {
  $f1 = null;
$f1 = function($x, $y) use (&$f1) {
  return 1.0 + $y * $y;
};
  $y1 = runge_kutta_fehlberg_45($f1, 0.0, 0.0, 0.2, 1.0);
  echo rtrim(json_encode($y1[1], 1344)), PHP_EOL;
  $f2 = null;
$f2 = function($x, $y) use (&$f2, $f1, $y1) {
  return $x;
};
  $y2 = runge_kutta_fehlberg_45($f2, -1.0, 0.0, 0.2, 0.0);
  echo rtrim(json_encode($y2[1], 1344)), PHP_EOL;
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
