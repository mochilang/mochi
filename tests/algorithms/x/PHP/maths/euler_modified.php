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
  function ceil_float($x) {
  $i = intval($x);
  if ($x > (floatval($i))) {
  return $i + 1;
}
  return $i;
};
  function exp_approx($x) {
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 20) {
  $term = $term * $x / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function euler_modified($ode_func, $y0, $x0, $step, $x_end) {
  $n = ceil_float(($x_end - $x0) / $step);
  $y = [$y0];
  $x = $x0;
  $k = 0;
  while ($k < $n) {
  $y_predict = $y[$k] + $step * $ode_func($x, $y[$k]);
  $slope1 = $ode_func($x, $y[$k]);
  $slope2 = $ode_func($x + $step, $y_predict);
  $y_next = $y[$k] + ($step / 2.0) * ($slope1 + $slope2);
  $y = _append($y, $y_next);
  $x = $x + $step;
  $k = $k + 1;
};
  return $y;
};
  function f1($x, $y) {
  return -2.0 * $x * $y * $y;
};
  function f2($x, $y) {
  return -2.0 * $y + ($x * $x * $x) * exp_approx(-2.0 * $x);
};
  function main() {
  $y1 = euler_modified('f1', 1.0, 0.0, 0.2, 1.0);
  echo rtrim(json_encode($y1[count($y1) - 1], 1344)), PHP_EOL;
  $y2 = euler_modified('f2', 1.0, 0.0, 0.1, 0.3);
  echo rtrim(json_encode($y2[count($y2) - 1], 1344)), PHP_EOL;
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
