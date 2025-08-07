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
  $PI_DIGITS = '1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679';
  function calculate_pi($limit) {
  global $PI_DIGITS;
  if ($limit < 0 || $limit > strlen($PI_DIGITS)) {
  $panic('limit out of range');
}
  return '3.' . substr($PI_DIGITS, 0, $limit - 0);
};
  function test_pi_generator() {
  global $PI_DIGITS;
  if (calculate_pi(15) != '3.141592653589793') {
  $panic('calculate_pi 15 failed');
}
};
  function main() {
  global $PI_DIGITS;
  test_pi_generator();
  echo rtrim(calculate_pi(50)), PHP_EOL;
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
