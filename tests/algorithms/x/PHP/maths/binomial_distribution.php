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
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function factorial($n) {
  if ($n < 0) {
  $panic('factorial is undefined for negative numbers');
}
  $result = 1;
  $i = 2;
  while ($i <= $n) {
  $result = $result * $i;
  $i = $i + 1;
};
  return $result;
};
  function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function binomial_distribution($successes, $trials, $prob) {
  if ($successes > $trials) {
  $panic('successes must be lower or equal to trials');
}
  if ($trials < 0 || $successes < 0) {
  $panic('the function is defined for non-negative integers');
}
  if (!(0.0 < $prob && $prob < 1.0)) {
  $panic('prob has to be in range of 1 - 0');
}
  $probability = pow_float($prob, $successes) * pow_float(1.0 - $prob, $trials - $successes);
  $numerator = floatval(factorial($trials));
  $denominator = floatval((factorial($successes) * factorial($trials - $successes)));
  $coefficient = $numerator / $denominator;
  return $probability * $coefficient;
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
