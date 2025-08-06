<?php
ini_set('memory_limit', '-1');
$time_chart = ['seconds' => 1.0, 'minutes' => 60.0, 'hours' => 3600.0, 'days' => 86400.0, 'weeks' => 604800.0, 'months' => 2629800.0, 'years' => 31557600.0];
$time_chart_inverse = ['seconds' => 1.0, 'minutes' => 1.0 / 60.0, 'hours' => 1.0 / 3600.0, 'days' => 1.0 / 86400.0, 'weeks' => 1.0 / 604800.0, 'months' => 1.0 / 2629800.0, 'years' => 1.0 / 31557600.0];
$units = ['seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years'];
$units_str = 'seconds, minutes, hours, days, weeks, months, years';
function contains($arr, $t) {
  global $time_chart, $time_chart_inverse, $units, $units_str;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $t) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function convert_time($time_value, $unit_from, $unit_to) {
  global $time_chart, $time_chart_inverse, $units, $units_str;
  if ($time_value < 0.0) {
  $panic('\'time_value\' must be a non-negative number.');
}
  $from = strtolower($unit_from);
  $to = strtolower($unit_to);
  if ((!in_array($from, $units)) || (!in_array($to, $units))) {
  $invalid_unit = $from;
  if (in_array($from, $units)) {
  $invalid_unit = $to;
};
  $panic('Invalid unit ' . $invalid_unit . ' is not in ' . $units_str . '.');
}
  $seconds = $time_value * $time_chart[$from];
  $converted = $seconds * $time_chart_inverse[$to];
  $scaled = $converted * 1000.0;
  $int_part = intval($scaled + 0.5);
  return ($int_part + 0.0) / 1000.0;
}
echo rtrim(json_encode(convert_time(3600.0, 'seconds', 'hours'), 1344)), PHP_EOL;
echo rtrim(json_encode(convert_time(360.0, 'days', 'months'), 1344)), PHP_EOL;
echo rtrim(json_encode(convert_time(360.0, 'months', 'years'), 1344)), PHP_EOL;
echo rtrim(json_encode(convert_time(1.0, 'years', 'seconds'), 1344)), PHP_EOL;
