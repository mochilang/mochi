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
  function double_factorial_recursive($n) {
  if ($n < 0) {
  $panic('double_factorial_recursive() not defined for negative values');
}
  if ($n <= 1) {
  return 1;
}
  return $n * double_factorial_recursive($n - 2);
};
  function double_factorial_iterative($n) {
  if ($n < 0) {
  $panic('double_factorial_iterative() not defined for negative values');
}
  $result = 1;
  $i = $n;
  while ($i > 0) {
  $result = $result * $i;
  $i = $i - 2;
};
  return $result;
};
  function test_double_factorial() {
  if (double_factorial_recursive(0) != 1) {
  $panic('0!! recursive failed');
}
  if (double_factorial_iterative(0) != 1) {
  $panic('0!! iterative failed');
}
  if (double_factorial_recursive(1) != 1) {
  $panic('1!! recursive failed');
}
  if (double_factorial_iterative(1) != 1) {
  $panic('1!! iterative failed');
}
  if (double_factorial_recursive(5) != 15) {
  $panic('5!! recursive failed');
}
  if (double_factorial_iterative(5) != 15) {
  $panic('5!! iterative failed');
}
  if (double_factorial_recursive(6) != 48) {
  $panic('6!! recursive failed');
}
  if (double_factorial_iterative(6) != 48) {
  $panic('6!! iterative failed');
}
  $n = 0;
  while ($n <= 10) {
  if (double_factorial_recursive($n) != double_factorial_iterative($n)) {
  $panic('double factorial mismatch');
}
  $n = $n + 1;
};
};
  function main() {
  test_double_factorial();
  echo rtrim(json_encode(double_factorial_iterative(10), 1344)), PHP_EOL;
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
