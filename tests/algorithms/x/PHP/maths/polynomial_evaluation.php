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
  function pow_float($base, $exponent) {
  $exp = $exponent;
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function evaluate_poly($poly, $x) {
  $total = 0.0;
  $i = 0;
  while ($i < count($poly)) {
  $total = $total + $poly[$i] * pow_float($x, $i);
  $i = $i + 1;
};
  return $total;
};
  function horner($poly, $x) {
  $result = 0.0;
  $i = count($poly) - 1;
  while ($i >= 0) {
  $result = $result * $x + $poly[$i];
  $i = $i - 1;
};
  return $result;
};
  function test_polynomial_evaluation() {
  $poly = [0.0, 0.0, 5.0, 9.3, 7.0];
  $x = 10.0;
  if (evaluate_poly($poly, $x) != 79800.0) {
  $panic('evaluate_poly failed');
}
  if (horner($poly, $x) != 79800.0) {
  $panic('horner failed');
}
};
  function main() {
  test_polynomial_evaluation();
  $poly = [0.0, 0.0, 5.0, 9.3, 7.0];
  $x = 10.0;
  echo rtrim(json_encode(evaluate_poly($poly, $x), 1344)), PHP_EOL;
  echo rtrim(json_encode(horner($poly, $x), 1344)), PHP_EOL;
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
