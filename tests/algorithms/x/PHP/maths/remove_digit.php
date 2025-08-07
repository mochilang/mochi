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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function remove_digit($num) {
  $n = $num;
  if ($n < 0) {
  $n = -$n;
}
  $max_val = 0;
  $divisor = 1;
  while ($divisor <= $n) {
  $higher = _intdiv($n, ($divisor * 10));
  $lower = $n % $divisor;
  $candidate = $higher * $divisor + $lower;
  if ($candidate > $max_val) {
  $max_val = $candidate;
}
  $divisor = $divisor * 10;
};
  return $max_val;
};
  function test_remove_digit() {
  if (remove_digit(152) != 52) {
  $panic('remove_digit(152) failed');
}
  if (remove_digit(6385) != 685) {
  $panic('remove_digit(6385) failed');
}
  if (remove_digit(-11) != 1) {
  $panic('remove_digit(-11) failed');
}
  if (remove_digit(2222222) != 222222) {
  $panic('remove_digit(2222222) failed');
}
};
  function main() {
  test_remove_digit();
  echo rtrim(json_encode(remove_digit(152), 1344)), PHP_EOL;
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
