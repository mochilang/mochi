<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  function factorial($n) {
  if ($n < 0) {
  _panic('factorial() not defined for negative values');
}
  $value = 1;
  $i = 1;
  while ($i <= $n) {
  $value = $value * $i;
  $i = $i + 1;
};
  return $value;
};
  function factorial_recursive($n) {
  if ($n < 0) {
  _panic('factorial() not defined for negative values');
}
  if ($n <= 1) {
  return 1;
}
  return $n * factorial_recursive($n - 1);
};
  function test_zero() {
  if (factorial(0) != 1) {
  _panic('factorial(0) failed');
}
  if (factorial_recursive(0) != 1) {
  _panic('factorial_recursive(0) failed');
}
};
  function test_positive_integers() {
  if (factorial(1) != 1) {
  _panic('factorial(1) failed');
}
  if (factorial_recursive(1) != 1) {
  _panic('factorial_recursive(1) failed');
}
  if (factorial(5) != 120) {
  _panic('factorial(5) failed');
}
  if (factorial_recursive(5) != 120) {
  _panic('factorial_recursive(5) failed');
}
  if (factorial(7) != 5040) {
  _panic('factorial(7) failed');
}
  if (factorial_recursive(7) != 5040) {
  _panic('factorial_recursive(7) failed');
}
};
  function test_large_number() {
  if (factorial(10) != 3628800) {
  _panic('factorial(10) failed');
}
  if (factorial_recursive(10) != 3628800) {
  _panic('factorial_recursive(10) failed');
}
};
  function run_tests() {
  test_zero();
  test_positive_integers();
  test_large_number();
};
  function main() {
  run_tests();
  echo rtrim(json_encode(factorial(6), 1344)), PHP_EOL;
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
