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
  function factorial($n) {
  if ($n < 0) {
  $panic('factorial() not defined for negative values');
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
  $panic('factorial() not defined for negative values');
}
  if ($n <= 1) {
  return 1;
}
  return $n * factorial_recursive($n - 1);
};
  function test_factorial() {
  $i = 0;
  while ($i <= 10) {
  if (factorial($i) != factorial_recursive($i)) {
  $panic('mismatch between factorial and factorial_recursive');
}
  $i = $i + 1;
};
  if (factorial(6) != 720) {
  $panic('factorial(6) should be 720');
}
};
  function main() {
  test_factorial();
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
