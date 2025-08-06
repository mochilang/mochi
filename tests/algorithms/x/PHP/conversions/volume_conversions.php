<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
$units = ['cubic meter', 'litre', 'kilolitre', 'gallon', 'cubic yard', 'cubic foot', 'cup'];
$from_factors = [1.0, 0.001, 1.0, 0.00454, 0.76455, 0.028, 0.000236588];
$to_factors = [1.0, 1000.0, 1.0, 264.172, 1.30795, 35.3147, 4226.75];
function supported_values() {
  global $units, $from_factors, $to_factors;
  $result = $units[0];
  $i = 1;
  while ($i < count($units)) {
  $result = $result . ', ' . $units[$i];
  $i = $i + 1;
};
  return $result;
}
function find_index($name) {
  global $units, $from_factors, $to_factors;
  $i = 0;
  while ($i < count($units)) {
  if ($units[$i] == $name) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function get_from_factor($name) {
  global $units, $from_factors, $to_factors;
  $idx = find_index($name);
  if ($idx < 0) {
  $panic('Invalid \'from_type\' value: \'' . $name . '\' Supported values are: ' . supported_values());
}
  return $from_factors[$idx];
}
function get_to_factor($name) {
  global $units, $from_factors, $to_factors;
  $idx = find_index($name);
  if ($idx < 0) {
  $panic('Invalid \'to_type\' value: \'' . $name . '\' Supported values are: ' . supported_values());
}
  return $to_factors[$idx];
}
function volume_conversion($value, $from_type, $to_type) {
  global $units, $from_factors, $to_factors;
  $from_factor = get_from_factor($from_type);
  $to_factor = get_to_factor($to_type);
  return $value * $from_factor * $to_factor;
}
echo rtrim(_str(volume_conversion(4.0, 'cubic meter', 'litre'))), PHP_EOL;
echo rtrim(_str(volume_conversion(1.0, 'litre', 'gallon'))), PHP_EOL;
echo rtrim(_str(volume_conversion(1.0, 'kilolitre', 'cubic meter'))), PHP_EOL;
echo rtrim(_str(volume_conversion(3.0, 'gallon', 'cubic yard'))), PHP_EOL;
echo rtrim(_str(volume_conversion(2.0, 'cubic yard', 'litre'))), PHP_EOL;
echo rtrim(_str(volume_conversion(4.0, 'cubic foot', 'cup'))), PHP_EOL;
echo rtrim(_str(volume_conversion(1.0, 'cup', 'kilolitre'))), PHP_EOL;
