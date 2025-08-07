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
  function signum($num) {
  if ($num < 0.0) {
  return -1;
}
  if ($num > 0.0) {
  return 1;
}
  return 0;
};
  function test_signum() {
  if (signum(5.0) != 1) {
  $panic('signum(5) failed');
}
  if (signum(-5.0) != (-1)) {
  $panic('signum(-5) failed');
}
  if (signum(0.0) != 0) {
  $panic('signum(0) failed');
}
  if (signum(10.5) != 1) {
  $panic('signum(10.5) failed');
}
  if (signum(-10.5) != (-1)) {
  $panic('signum(-10.5) failed');
}
  if (signum(0.000001) != 1) {
  $panic('signum(1e-6) failed');
}
  if (signum(-0.000001) != (-1)) {
  $panic('signum(-1e-6) failed');
}
  if (signum(123456789.0) != 1) {
  $panic('signum(123456789) failed');
}
  if (signum(-123456789.0) != (-1)) {
  $panic('signum(-123456789) failed');
}
};
  function main() {
  test_signum();
  echo rtrim(json_encode(signum(12.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(signum(-12.0), 1344)), PHP_EOL;
  echo rtrim(json_encode(signum(0.0), 1344)), PHP_EOL;
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
