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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function find_previous_power_of_two($number) {
  if ($number < 0) {
  $panic('Input must be a non-negative integer');
}
  if ($number == 0) {
  return 0;
}
  $power = 1;
  while ($power <= $number) {
  $power = $power * 2;
};
  if ($number > 1) {
  return _intdiv($power, 2);
} else {
  return 1;
}
}
function main() {
  $results = [];
  $i = 0;
  while ($i < 18) {
  $results = _append($results, find_previous_power_of_two($i));
  $i = $i + 1;
};
  echo rtrim(_str($results)), PHP_EOL;
}
main();
