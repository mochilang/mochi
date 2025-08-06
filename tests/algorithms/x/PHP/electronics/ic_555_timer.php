<?php
ini_set('memory_limit', '-1');
function astable_frequency($resistance_1, $resistance_2, $capacitance) {
  if ($resistance_1 <= 0.0 || $resistance_2 <= 0.0 || $capacitance <= 0.0) {
  $panic('All values must be positive');
}
  return (1.44 / (($resistance_1 + 2.0 * $resistance_2) * $capacitance)) * 1000000.0;
}
function astable_duty_cycle($resistance_1, $resistance_2) {
  if ($resistance_1 <= 0.0 || $resistance_2 <= 0.0) {
  $panic('All values must be positive');
}
  return ($resistance_1 + $resistance_2) / ($resistance_1 + 2.0 * $resistance_2) * 100.0;
}
echo rtrim(json_encode(astable_frequency(45.0, 45.0, 7.0), 1344)), PHP_EOL;
echo rtrim(json_encode(astable_duty_cycle(45.0, 45.0), 1344)), PHP_EOL;
