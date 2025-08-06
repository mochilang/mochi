<?php
ini_set('memory_limit', '-1');
$PRESSURE_CONVERSION = ['atm' => ['from_factor' => 1.0, 'to_factor' => 1.0], 'pascal' => ['from_factor' => 0.0000098, 'to_factor' => 101325.0], 'bar' => ['from_factor' => 0.986923, 'to_factor' => 1.01325], 'kilopascal' => ['from_factor' => 0.00986923, 'to_factor' => 101.325], 'megapascal' => ['from_factor' => 9.86923, 'to_factor' => 0.101325], 'psi' => ['from_factor' => 0.068046, 'to_factor' => 14.6959], 'inHg' => ['from_factor' => 0.0334211, 'to_factor' => 29.9213], 'torr' => ['from_factor' => 0.00131579, 'to_factor' => 760.0]];
function pressure_conversion($value, $from_type, $to_type) {
  global $PRESSURE_CONVERSION;
  if (!(array_key_exists($from_type, $PRESSURE_CONVERSION))) {
  $keys = array_keys($PRESSURE_CONVERSION);
  $panic('Invalid \'from_type\' value: \'' . $from_type . '\'  Supported values are:
' . $keys);
}
  if (!(array_key_exists($to_type, $PRESSURE_CONVERSION))) {
  $keys = array_keys($PRESSURE_CONVERSION);
  $panic('Invalid \'to_type\' value: \'' . $to_type . '.  Supported values are:
' . $keys);
}
  $from = $PRESSURE_CONVERSION[$from_type];
  $to = $PRESSURE_CONVERSION[$to_type];
  return $value * $from['from_factor'] * $to['to_factor'];
}
echo rtrim(json_encode(pressure_conversion(4.0, 'atm', 'pascal'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(1.0, 'pascal', 'psi'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(1.0, 'bar', 'atm'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(3.0, 'kilopascal', 'bar'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(2.0, 'megapascal', 'psi'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(4.0, 'psi', 'torr'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(1.0, 'inHg', 'atm'), 1344)), PHP_EOL;
echo rtrim(json_encode(pressure_conversion(1.0, 'torr', 'psi'), 1344)), PHP_EOL;
