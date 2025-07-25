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
  function sinApprox($x) {
  global $PI, $dt, $s, $t1, $k1, $i, $t2, $k2, $i2;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 12) {
  $denom = floatval(((2 * $n) * (2 * $n + 1)));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  $dt = 0.01;
  $s = 0.0;
  $t1 = 0.0;
  $k1 = sinApprox(0.0);
  $i = 1;
  while ($i <= 200) {
  $t2 = (floatval($i)) * $dt;
  $k2 = sinApprox($t2 * $PI);
  $s = $s + ($k1 + $k2) * 0.5 * ($t2 - $t1);
  $t1 = $t2;
  $k1 = $k2;
  $i = $i + 1;
}
  $i2 = 1;
  while ($i2 <= 50) {
  $t2 = 2.0 + (floatval($i2)) * $dt;
  $k2 = 0.0;
  $s = $s + ($k1 + $k2) * 0.5 * ($t2 - $t1);
  $t1 = $t2;
  $k1 = $k2;
  $i2 = $i2 + 1;
}
  echo rtrim(json_encode($s, 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
