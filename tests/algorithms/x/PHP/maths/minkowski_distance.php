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
  function abs_val($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function nth_root($value, $n) {
  if ($value == 0.0) {
  return 0.0;
}
  $x = $value / (floatval($n));
  $i = 0;
  while ($i < 20) {
  $num = (floatval(($n - 1))) * $x + $value / pow_float($x, $n - 1);
  $x = $num / (floatval($n));
  $i = $i + 1;
};
  return $x;
};
  function minkowski_distance($point_a, $point_b, $order) {
  if ($order < 1) {
  $panic('The order must be greater than or equal to 1.');
}
  if (count($point_a) != count($point_b)) {
  $panic('Both points must have the same dimension.');
}
  $total = 0.0;
  $idx = 0;
  while ($idx < count($point_a)) {
  $diff = abs_val($point_a[$idx] - $point_b[$idx]);
  $total = $total + pow_float($diff, $order);
  $idx = $idx + 1;
};
  return nth_root($total, $order);
};
  function test_minkowski() {
  if (abs_val(minkowski_distance([1.0, 1.0], [2.0, 2.0], 1) - 2.0) > 0.0001) {
  $panic('minkowski_distance test1 failed');
}
  if (abs_val(minkowski_distance([1.0, 2.0, 3.0, 4.0], [5.0, 6.0, 7.0, 8.0], 2) - 8.0) > 0.0001) {
  $panic('minkowski_distance test2 failed');
}
};
  function main() {
  test_minkowski();
  echo rtrim(json_encode(minkowski_distance([5.0], [0.0], 3), 1344)), PHP_EOL;
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
