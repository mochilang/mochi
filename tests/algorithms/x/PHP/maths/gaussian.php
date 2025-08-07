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
  $PI = 3.141592653589793;
  function sqrtApprox($x) {
  global $PI;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function expApprox($x) {
  global $PI;
  $is_neg = false;
  $y = $x;
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
  function gaussian($x, $mu, $sigma) {
  global $PI;
  $coeff = 1.0 / sqrtApprox(2.0 * $PI * $sigma * $sigma);
  $exponent = -(($x - $mu) * ($x - $mu)) / (2.0 * $sigma * $sigma);
  return $coeff * expApprox($exponent);
};
  function main() {
  global $PI;
  $result = gaussian(1.0, 0.0, 1.0);
  echo rtrim(json_encode($result, 1344)), PHP_EOL;
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
