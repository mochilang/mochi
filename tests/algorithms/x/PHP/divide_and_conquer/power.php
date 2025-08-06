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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function actual_power($a, $b) {
  if ($b == 0) {
  return 1;
}
  $half = actual_power($a, _intdiv($b, 2));
  if ($b % 2 == 0) {
  return $half * $half;
}
  return $a * $half * $half;
}
function power($a, $b) {
  if ($b < 0) {
  return 1.0 / (1.0 * actual_power($a, -$b));
}
  return 1.0 * actual_power($a, $b);
}
echo rtrim(_str(power(4, 6))), PHP_EOL;
echo rtrim(_str(power(2, 3))), PHP_EOL;
echo rtrim(_str(power(-2, 3))), PHP_EOL;
echo rtrim(_str(power(2, -3))), PHP_EOL;
echo rtrim(_str(power(-2, -3))), PHP_EOL;
