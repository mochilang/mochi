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
  $seed = 1;
  function mochi_rand() {
  global $seed, $INITIAL_VALUE, $result;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function randint($low, $high) {
  global $seed, $INITIAL_VALUE, $result;
  return (fmod(mochi_rand(), ($high - $low + 1))) + $low;
};
  function expApprox($x) {
  global $seed, $INITIAL_VALUE, $result;
  $y = $x;
  $is_neg = false;
  if ($x < 0.0) {
  $is_neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
};
  function sigmoid($x) {
  global $seed, $INITIAL_VALUE, $result;
  return 1.0 / (1.0 + expApprox(-$x));
};
  function sigmoid_derivative($sig_val) {
  global $seed, $INITIAL_VALUE, $result;
  return $sig_val * (1.0 - $sig_val);
};
  $INITIAL_VALUE = 0.02;
  function forward_propagation($expected, $number_propagations) {
  global $seed, $INITIAL_VALUE, $result;
  $weight = 2.0 * (floatval(randint(1, 100))) - 1.0;
  $layer_1 = 0.0;
  $i = 0;
  while ($i < $number_propagations) {
  $layer_1 = sigmoid($INITIAL_VALUE * $weight);
  $layer_1_error = (floatval($expected) / 100.0) - $layer_1;
  $layer_1_delta = $layer_1_error * sigmoid_derivative($layer_1);
  $weight = $weight + $INITIAL_VALUE * $layer_1_delta;
  $i = $i + 1;
};
  return $layer_1 * 100.0;
};
  $seed = 1;
  $result = forward_propagation(32, 450000);
  echo rtrim(json_encode($result, 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
