<?php
function mochi_abs($x) {
  global $sqrtApprox, $agmPi, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $mochi_abs, $agmPi, $main;
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function agmPi() {
  global $mochi_abs, $sqrtApprox, $main;
  $a = 1.0;
  $g = 1.0 / sqrtApprox(2.0);
  $sum = 0.0;
  $pow = 2.0;
  while (mochi_abs($a - $g) > 0.000000000000001) {
  $t = ($a + $g) / 2.0;
  $u = sqrtApprox($a * $g);
  $a = $t;
  $g = $u;
  $pow = $pow * 2.0;
  $diff = $a * $a - $g * $g;
  $sum = $sum + $diff * $pow;
};
  $pi = 4.0 * $a * $a / (1.0 - $sum);
  return $pi;
}
function main() {
  global $mochi_abs, $sqrtApprox, $agmPi;
  echo json_encode(agmPi(), 1344), PHP_EOL;
}
main();
