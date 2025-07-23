<?php
function mochi_abs($x) {
  global $maxf, $isClose, $sqrtApprox, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function maxf($a, $b) {
  global $mochi_abs, $isClose, $sqrtApprox, $main;
  if ($a > $b) {
  return $a;
}
  return $b;
}
function isClose($a, $b) {
  global $mochi_abs, $maxf, $sqrtApprox, $main;
  $relTol = 0.000000001;
  $t = mochi_abs($a - $b);
  $u = $relTol * maxf(mochi_abs($a), mochi_abs($b));
  return $t <= $u;
}
function sqrtApprox($x) {
  global $mochi_abs, $maxf, $isClose, $main;
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function main() {
  global $mochi_abs, $maxf, $isClose, $sqrtApprox;
  $root2 = sqrtApprox(2.0);
  $pairs = [[100000000000000.02, 100000000000000.02], [100.01, 100.011], [10000000000000.002 / 10000.0, 1000000000.0000001], [0.001, 0.0010000001], [0.000000000000000000000101, 0.0], [$root2 * $root2, 2.0], [(-$root2) * $root2, -2.0], [100000000000000000.0, 100000000000000000.0], [3.141592653589793, 3.141592653589793]];
  foreach ($pairs as $pair) {
  $a = $pair[0];
  $b = $pair[1];
  $s = (isClose($a, $b) ? "≈" : "≉");
  echo json_encode($a, 1344) . " " . $s . " " . json_encode($b, 1344), PHP_EOL;
};
}
main();
