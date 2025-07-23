<?php
$PI = 3.141592653589793;
function sinApprox($x) {
  global $PI, $cosApprox, $degreesIncr, $turns, $stop, $width, $centre, $a, $b, $theta, $count, $r, $y;
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
  global $PI, $sinApprox, $degreesIncr, $turns, $stop, $width, $centre, $a, $b, $theta, $count, $r, $y;
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
$degreesIncr = 0.1 * $PI / 180.0;
$turns = 2.0;
$stop = 360.0 * $turns * 10.0 * $degreesIncr;
$width = 600.0;
$centre = $width / 2.0;
$a = 1.0;
$b = 20.0;
$theta = 0.0;
$count = 0;
while ($theta < $stop) {
  $r = $a + $b * $theta;
  $x = $r * cosApprox($theta);
  $y = $r * sinApprox($theta);
  if ($count % 100 == 0) {
  echo json_encode($centre + $x, 1344) . "," . json_encode($centre - $y, 1344), PHP_EOL;
}
  $theta = $theta + $degreesIncr;
  $count = $count + 1;
}
