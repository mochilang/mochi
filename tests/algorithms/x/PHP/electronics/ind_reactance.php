<?php
ini_set('memory_limit', '-1');
$PI = 3.141592653589793;
function ind_reactance($inductance, $frequency, $reactance) {
  global $PI;
  $zero_count = 0;
  if ($inductance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($frequency == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($reactance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  $panic('One and only one argument must be 0');
}
  if ($inductance < 0.0) {
  $panic('Inductance cannot be negative');
}
  if ($frequency < 0.0) {
  $panic('Frequency cannot be negative');
}
  if ($reactance < 0.0) {
  $panic('Inductive reactance cannot be negative');
}
  if ($inductance == 0.0) {
  return ['inductance' => $reactance / (2.0 * $PI * $frequency)];
}
  if ($frequency == 0.0) {
  return ['frequency' => $reactance / (2.0 * $PI * $inductance)];
}
  return ['reactance' => 2.0 * $PI * $frequency * $inductance];
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.0, 10000.0, 50.0), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.035, 0.0, 50.0), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(ind_reactance(0.000035, 1000.0, 0.0), 1344)))))), PHP_EOL;
