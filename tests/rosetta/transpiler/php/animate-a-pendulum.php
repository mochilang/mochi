<?php
$PI = 3.141592653589793;
function sinApprox($x) {
  global $PI, $cosApprox, $sqrtApprox, $L, $G, $dt, $phi0, $omega, $t, $phi, $pos;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 10) {
  $denom = ((2 * $n) * (2 * $n + 1));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function cosApprox($x) {
  global $PI, $sinApprox, $sqrtApprox, $L, $G, $dt, $phi0, $omega, $t, $phi, $pos;
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n <= 10) {
  $denom = ((2 * $n - 1) * (2 * $n));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
function sqrtApprox($x) {
  global $PI, $sinApprox, $cosApprox, $L, $G, $dt, $phi0, $omega, $t, $phi, $pos;
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
$L = 10.0;
$G = 9.81;
$dt = 0.2;
$phi0 = $PI / 4.0;
$omega = sqrtApprox($G / $L);
$t = 0.0;
for ($step = 0; $step < 10; $step++) {
  $phi = $phi0 * cosApprox($omega * $t);
  $pos = intval((10.0 * sinApprox($phi) + 0.5));
  echo json_encode($pos, 1344), PHP_EOL;
  $t = $t + $dt;
}
