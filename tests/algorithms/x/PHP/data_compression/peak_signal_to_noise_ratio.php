<?php
ini_set('memory_limit', '-1');
function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function ln($x) {
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return 2.0 * $sum;
}
function log10($x) {
  return ln($x) / ln(10.0);
}
function peak_signal_to_noise_ratio($original, $contrast) {
  $mse = 0.0;
  $i = 0;
  while ($i < count($original)) {
  $j = 0;
  while ($j < count($original[$i])) {
  $diff = floatval(($original[$i][$j] - $contrast[$i][$j]));
  $mse = $mse + $diff * $diff;
  $j = $j + 1;
};
  $i = $i + 1;
};
  $size = floatval((count($original) * count($original[0])));
  $mse = $mse / $size;
  if ($mse == 0.0) {
  return 100.0;
}
  $PIXEL_MAX = 255.0;
  return 20.0 * log10($PIXEL_MAX / sqrtApprox($mse));
}
