<?php
function mochi_abs($x) {
  global $sqrtApprox, $agm, $main;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $mochi_abs, $agm, $main;
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function agm($a, $g) {
  global $mochi_abs, $sqrtApprox, $main;
  $eps = 0.00000000000001;
  while (mochi_abs($a - $g) > mochi_abs($a) * $eps) {
  $newA = ($a + $g) / 2.0;
  $newG = sqrtApprox($a * $g);
  $a = $newA;
  $g = $newG;
};
  return $a;
}
function main() {
  global $mochi_abs, $sqrtApprox, $agm;
  echo json_encode(agm(1.0, 1.0 / sqrtApprox(2.0)), 1344), PHP_EOL;
}
main();
