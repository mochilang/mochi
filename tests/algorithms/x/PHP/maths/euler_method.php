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
  function ceil_int($x) {
  $n = intval($x);
  if (floatval($n) < $x) {
  $n = $n + 1;
}
  return $n;
};
  function explicit_euler($ode_func, $y0, $x0, $step_size, $x_end) {
  $n = ceil_int(($x_end - $x0) / $step_size);
  $y = [];
  $i = 0;
  while ($i <= $n) {
  $y = _append($y, 0.0);
  $i = $i + 1;
};
  $y[0] = $y0;
  $x = $x0;
  $k = 0;
  while ($k < $n) {
  $y[$k + 1] = $y[$k] + $step_size * $ode_func($x, $y[$k]);
  $x = $x + $step_size;
  $k = $k + 1;
};
  return $y;
};
  function abs_float($a) {
  if ($a < 0.0) {
  return -$a;
}
  return $a;
};
  function test_explicit_euler() {
  $f = null;
$f = function($x, $y) use ($f) {
  return $y;
};
  $ys = explicit_euler($f, 1.0, 0.0, 0.01, 5.0);
  $last = $ys[count($ys) - 1];
  if (abs_float($last - 144.77277243257308) > 0.001) {
  $panic('explicit_euler failed');
}
};
  function main() {
  test_explicit_euler();
  $f = null;
$f = function($x, $y) use ($f) {
  return $y;
};
  $ys = explicit_euler($f, 1.0, 0.0, 0.01, 5.0);
  echo rtrim(json_encode($ys[count($ys) - 1], 1344)), PHP_EOL;
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
