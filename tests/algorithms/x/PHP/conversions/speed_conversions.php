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
$units = ['km/h', 'm/s', 'mph', 'knot'];
$speed_chart = [1.0, 3.6, 1.609344, 1.852];
$speed_chart_inverse = [1.0, 0.277777778, 0.621371192, 0.539956803];
function index_of($arr, $value) {
  global $units, $speed_chart, $speed_chart_inverse;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $value) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function units_string($arr) {
  global $units, $speed_chart, $speed_chart_inverse;
  $s = '';
  $i = 0;
  while ($i < count($arr)) {
  if ($i > 0) {
  $s = $s . ', ';
}
  $s = $s . $arr[$i];
  $i = $i + 1;
};
  return $s;
}
function round3($x) {
  global $units, $speed_chart, $speed_chart_inverse;
  $y = $x * 1000.0 + 0.5;
  $z = intval($y);
  $zf = floatval($z);
  return $zf / 1000.0;
}
function convert_speed($speed, $unit_from, $unit_to) {
  global $units, $speed_chart, $speed_chart_inverse;
  $from_index = index_of($units, $unit_from);
  $to_index = index_of($units, $unit_to);
  if ($from_index < 0 || $to_index < 0) {
  $msg = 'Incorrect \'from_type\' or \'to_type\' value: ' . $unit_from . ', ' . $unit_to . '
Valid values are: ' . units_string($units);
  $panic($msg);
}
  $result = $speed * $speed_chart[$from_index] * $speed_chart_inverse[$to_index];
  $r = round3($result);
  return $r;
}
echo rtrim(_str(convert_speed(100.0, 'km/h', 'm/s'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'km/h', 'mph'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'km/h', 'knot'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'm/s', 'km/h'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'm/s', 'mph'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'm/s', 'knot'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'mph', 'km/h'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'mph', 'm/s'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'mph', 'knot'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'knot', 'km/h'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'knot', 'm/s'))), PHP_EOL;
echo rtrim(_str(convert_speed(100.0, 'knot', 'mph'))), PHP_EOL;
