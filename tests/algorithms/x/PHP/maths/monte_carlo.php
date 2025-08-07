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
  $rand_seed = 123456789;
  function rand_float() {
  global $PI, $rand_seed;
  $rand_seed = (1103515245 * $rand_seed + 12345) % 2147483648;
  return (floatval($rand_seed)) / 2147483648.0;
};
  function rand_range($min_val, $max_val) {
  global $PI, $rand_seed;
  return rand_float() * ($max_val - $min_val) + $min_val;
};
  function abs_float($x) {
  global $PI, $rand_seed;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrtApprox($x) {
  global $PI, $rand_seed;
  if ($x == 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function pi_estimator($iterations) {
  global $PI, $rand_seed;
  $inside = 0.0;
  $i = 0;
  while ($i < $iterations) {
  $x = rand_range(-1.0, 1.0);
  $y = rand_range(-1.0, 1.0);
  if ($x * $x + $y * $y <= 1.0) {
  $inside = $inside + 1.0;
}
  $i = $i + 1;
};
  $proportion = $inside / (floatval($iterations));
  $pi_estimate = $proportion * 4.0;
  echo rtrim('The estimated value of pi is') . " " . rtrim(json_encode($pi_estimate, 1344)), PHP_EOL;
  echo rtrim('The numpy value of pi is') . " " . rtrim(json_encode($PI, 1344)), PHP_EOL;
  echo rtrim('The total error is') . " " . rtrim(json_encode(abs_float($PI - $pi_estimate), 1344)), PHP_EOL;
};
  function area_under_curve_estimator($iterations, $f, $min_value, $max_value) {
  global $PI, $rand_seed;
  $sum = 0.0;
  $i = 0;
  while ($i < $iterations) {
  $x = rand_range($min_value, $max_value);
  $sum = $sum + $f($x);
  $i = $i + 1;
};
  $expected = $sum / (floatval($iterations));
  return $expected * ($max_value - $min_value);
};
  function area_under_line_estimator_check($iterations, $min_value, $max_value) {
  global $PI, $rand_seed;
  $identity_function = null;
$identity_function = function($x) use (&$identity_function, $iterations, $min_value, $max_value, &$PI, &$rand_seed) {
  return $x;
};
  $estimated_value = area_under_curve_estimator($iterations, $identity_function, $min_value, $max_value);
  $expected_value = ($max_value * $max_value - $min_value * $min_value) / 2.0;
  echo rtrim('******************'), PHP_EOL;
  echo rtrim('Estimating area under y=x where x varies from') . " " . rtrim(json_encode($min_value, 1344)), PHP_EOL;
  echo rtrim('Estimated value is') . " " . rtrim(json_encode($estimated_value, 1344)), PHP_EOL;
  echo rtrim('Expected value is') . " " . rtrim(json_encode($expected_value, 1344)), PHP_EOL;
  echo rtrim('Total error is') . " " . rtrim(json_encode(abs_float($estimated_value - $expected_value), 1344)), PHP_EOL;
  echo rtrim('******************'), PHP_EOL;
};
  function pi_estimator_using_area_under_curve($iterations) {
  global $PI, $rand_seed;
  $semi_circle = null;
$semi_circle = function($x) use (&$semi_circle, $iterations, &$PI, &$rand_seed) {
  $y = 4.0 - $x * $x;
  $s = sqrtApprox($y);
  return $s;
};
  $estimated_value = area_under_curve_estimator($iterations, $semi_circle, 0.0, 2.0);
  echo rtrim('******************'), PHP_EOL;
  echo rtrim('Estimating pi using area_under_curve_estimator'), PHP_EOL;
  echo rtrim('Estimated value is') . " " . rtrim(json_encode($estimated_value, 1344)), PHP_EOL;
  echo rtrim('Expected value is') . " " . rtrim(json_encode($PI, 1344)), PHP_EOL;
  echo rtrim('Total error is') . " " . rtrim(json_encode(abs_float($estimated_value - $PI), 1344)), PHP_EOL;
  echo rtrim('******************'), PHP_EOL;
};
  function main() {
  global $PI, $rand_seed;
  pi_estimator(1000);
  area_under_line_estimator_check(1000, 0.0, 1.0);
  pi_estimator_using_area_under_curve(1000);
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
