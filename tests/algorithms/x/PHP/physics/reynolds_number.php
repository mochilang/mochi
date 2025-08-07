<?php
ini_set('memory_limit', '-1');
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function fabs($x) {
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
}
function reynolds_number($density, $velocity, $diameter, $viscosity) {
  if ($density <= 0.0 || $diameter <= 0.0 || $viscosity <= 0.0) {
  _panic('please ensure that density, diameter and viscosity are positive');
}
  return ($density * fabs($velocity) * $diameter) / $viscosity;
}
echo rtrim(json_encode(reynolds_number(900.0, 2.5, 0.05, 0.4), 1344)), PHP_EOL;
echo rtrim(json_encode(reynolds_number(450.0, 3.86, 0.078, 0.23), 1344)), PHP_EOL;
echo rtrim(json_encode(reynolds_number(234.0, -4.5, 0.3, 0.44), 1344)), PHP_EOL;
