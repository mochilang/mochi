<?php
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function pow_int($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
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
}
function round_nearest($x) {
  if ($x >= 0.0) {
  $n = intval(($x + 0.5));
  return floatval($n);
}
  $n = intval(($x - 0.5));
  return floatval($n);
}
function compute_geometric_mean($nums) {
  if (count($nums) == 0) {
  _panic('no numbers');
}
  $product = 1.0;
  $i = 0;
  while ($i < count($nums)) {
  $product = $product * $nums[$i];
  $i = $i + 1;
};
  if ($product < 0.0 && fmod(count($nums), 2) == 0) {
  _panic('Cannot Compute Geometric Mean for these numbers.');
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
}
function test_compute_geometric_mean() {
  $eps = 0.0001;
  $m1 = compute_geometric_mean([2.0, 8.0]);
  if (mochi_abs($m1 - 4.0) > $eps) {
  _panic('test1 failed');
}
  $m2 = compute_geometric_mean([5.0, 125.0]);
  if (mochi_abs($m2 - 25.0) > $eps) {
  _panic('test2 failed');
}
  $m3 = compute_geometric_mean([1.0, 0.0]);
  if (mochi_abs($m3 - 0.0) > $eps) {
  _panic('test3 failed');
}
  $m4 = compute_geometric_mean([1.0, 5.0, 25.0, 5.0]);
  if (mochi_abs($m4 - 5.0) > $eps) {
  _panic('test4 failed');
}
  $m5 = compute_geometric_mean([-5.0, 25.0, 1.0]);
  if (mochi_abs($m5 + 5.0) > $eps) {
  _panic('test5 failed');
}
}
function main() {
  test_compute_geometric_mean();
  echo rtrim(json_encode(compute_geometric_mean([-3.0, -27.0]), 1344)), PHP_EOL;
}
main();
