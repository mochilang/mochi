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
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function pow_int($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function nth_root($x, $n) {
  if ($x == 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $denom = pow_int($guess, $n - 1);
  $guess = (floatval(($n - 1)) * $guess + $x / $denom) / (floatval($n));
  $i = $i + 1;
};
  return $guess;
};
  function round_nearest($x) {
  if ($x >= 0.0) {
  $n = intval(($x + 0.5));
  return floatval($n);
}
  $n = intval(($x - 0.5));
  return floatval($n);
};
  function compute_geometric_mean($nums) {
  if (count($nums) == 0) {
  $panic('no numbers');
}
  $product = 1.0;
  $i = 0;
  while ($i < count($nums)) {
  $product = $product * $nums[$i];
  $i = $i + 1;
};
  if ($product < 0.0 && fmod(count($nums), 2) == 0) {
  $panic('Cannot Compute Geometric Mean for these numbers.');
}
  $mean = nth_root(mochi_abs($product), count($nums));
  if ($product < 0.0) {
  $mean = -$mean;
}
  $possible = round_nearest($mean);
  if (pow_int($possible, count($nums)) == $product) {
  $mean = $possible;
}
  return $mean;
};
  function test_compute_geometric_mean() {
  $eps = 0.0001;
  $m1 = compute_geometric_mean([2.0, 8.0]);
  if (mochi_abs($m1 - 4.0) > $eps) {
  $panic('test1 failed');
}
  $m2 = compute_geometric_mean([5.0, 125.0]);
  if (mochi_abs($m2 - 25.0) > $eps) {
  $panic('test2 failed');
}
  $m3 = compute_geometric_mean([1.0, 0.0]);
  if (mochi_abs($m3 - 0.0) > $eps) {
  $panic('test3 failed');
}
  $m4 = compute_geometric_mean([1.0, 5.0, 25.0, 5.0]);
  if (mochi_abs($m4 - 5.0) > $eps) {
  $panic('test4 failed');
}
  $m5 = compute_geometric_mean([-5.0, 25.0, 1.0]);
  if (mochi_abs($m5 + 5.0) > $eps) {
  $panic('test5 failed');
}
};
  function main() {
  test_compute_geometric_mean();
  echo rtrim(json_encode(compute_geometric_mean([-3.0, -27.0]), 1344)), PHP_EOL;
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
