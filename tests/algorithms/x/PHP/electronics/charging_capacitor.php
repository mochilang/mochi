<?php
ini_set('memory_limit', '-1');
function expApprox($x) {
  $y = $x;
  $is_neg = false;
  if ($x < 0.0) {
  $is_neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
}
function round3($x) {
  $scaled = $x * 1000.0;
  if ($scaled >= 0.0) {
  $scaled = $scaled + 0.5;
} else {
  $scaled = $scaled - 0.5;
}
  $scaled_int = intval($scaled);
  return (floatval($scaled_int)) / 1000.0;
}
function charging_capacitor($source_voltage, $resistance, $capacitance, $time_sec) {
  if ($source_voltage <= 0.0) {
  $panic('Source voltage must be positive.');
}
  if ($resistance <= 0.0) {
  $panic('Resistance must be positive.');
}
  if ($capacitance <= 0.0) {
  $panic('Capacitance must be positive.');
}
  $exponent = -$time_sec / ($resistance * $capacitance);
  $voltage = $source_voltage * (1.0 - expApprox($exponent));
  return round3($voltage);
}
echo rtrim(json_encode(charging_capacitor(0.2, 0.9, 8.4, 0.5), 1344)), PHP_EOL;
echo rtrim(json_encode(charging_capacitor(2.2, 3.5, 2.4, 9.0), 1344)), PHP_EOL;
echo rtrim(json_encode(charging_capacitor(15.0, 200.0, 20.0, 2.0), 1344)), PHP_EOL;
echo rtrim(json_encode(charging_capacitor(20.0, 2000.0, 0.0003, 4.0), 1344)), PHP_EOL;
