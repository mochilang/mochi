<?php
ini_set('memory_limit', '-1');
function sqrtApprox($x) {
  global $n, $sum, $rms;
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
$n = 10;
$sum = 0.0;
$x = 1;
while ($x <= $n) {
  $sum = $sum + (floatval($x)) * (floatval($x));
  $x = $x + 1;
}
$rms = sqrtApprox($sum / (floatval($n)));
echo rtrim(json_encode($rms, 1344)), PHP_EOL;
