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
  function power($base, $exponent) {
  if ($exponent == 0) {
  return 1;
}
  return $base * power($base, $exponent - 1);
};
  function test_power() {
  if (power(3, 4) != 81) {
  $panic('power(3,4) failed');
}
  if (power(2, 0) != 1) {
  $panic('power(2,0) failed');
}
  if (power(5, 6) != 15625) {
  $panic('power(5,6) failed');
}
};
  function main() {
  test_power();
  echo rtrim(json_encode(power(3, 4), 1344)), PHP_EOL;
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
