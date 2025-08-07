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
  function sqrt_newton($n) {
  if ($n == 0.0) {
  return 0.0;
}
  $x = $n;
  $i = 0;
  while ($i < 20) {
  $x = ($x + $n / $x) / 2.0;
  $i = $i + 1;
};
  return $x;
};
  function mochi_hypot($a, $b) {
  return sqrt_newton($a * $a + $b * $b);
};
  function line_length($fnc, $x_start, $x_end, $steps) {
  $x1 = $x_start;
  $fx1 = $fnc($x_start);
  $length = 0.0;
  $i = 0;
  $step = ($x_end - $x_start) / (1.0 * $steps);
  while ($i < $steps) {
  $x2 = $step + $x1;
  $fx2 = $fnc($x2);
  $length = $length + mochi_hypot($x2 - $x1, $fx2 - $fx1);
  $x1 = $x2;
  $fx1 = $fx2;
  $i = $i + 1;
};
  return $length;
};
  function f1($x) {
  return $x;
};
  function f2($x) {
  return 1.0;
};
  function f3($x) {
  return ($x * $x) / 10.0;
};
  echo rtrim(json_encode(line_length('f1', 0.0, 1.0, 10), 1344)), PHP_EOL;
  echo rtrim(json_encode(line_length('f2', -5.5, 4.5, 100), 1344)), PHP_EOL;
  echo rtrim(json_encode(line_length('f3', 0.0, 10.0, 1000), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
