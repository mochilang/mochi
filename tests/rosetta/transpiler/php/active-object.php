<?php
$PI = 3.141592653589793;
function sinApprox($x) {
  global $PI, $dt, $s, $t1, $k1, $i, $t2, $k2, $i2, $t2, $k2;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n <= 12) {
  $denom = ((2 * $n) * (2 * $n + 1));
  $term = -$term * $x * $x / $denom;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
}
$dt = 0.01;
$s = 0.0;
$t1 = 0.0;
$k1 = sinApprox(0.0);
$i = 1;
while ($i <= 200) {
  $t2 = ($i) * $dt;
  $k2 = sinApprox($t2 * $PI);
  $s = $s + ($k1 + $k2) * 0.5 * ($t2 - $t1);
  $t1 = $t2;
  $k1 = $k2;
  $i = $i + 1;
}
$i2 = 1;
while ($i2 <= 50) {
  $t2 = 2.0 + ($i2) * $dt;
  $k2 = 0.0;
  $s = $s + ($k1 + $k2) * 0.5 * ($t2 - $t1);
  $t1 = $t2;
  $k1 = $k2;
  $i2 = $i2 + 1;
}
echo json_encode($s, 1344), PHP_EOL;
