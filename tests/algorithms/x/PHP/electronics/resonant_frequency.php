<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
function sqrtApprox($x) {
  global $PI;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function resonant_frequency($inductance, $capacitance) {
  global $PI;
  if ($inductance <= 0.0) {
  $panic('Inductance cannot be 0 or negative');
}
  if ($capacitance <= 0.0) {
  $panic('Capacitance cannot be 0 or negative');
}
  $denom = 2.0 * $PI * sqrtApprox($inductance * $capacitance);
  return 1.0 / $denom;
}
echo rtrim(json_encode(resonant_frequency(10.0, 5.0), 1344)), PHP_EOL;
