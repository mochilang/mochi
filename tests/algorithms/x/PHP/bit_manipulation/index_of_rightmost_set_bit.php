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
function index_of_rightmost_set_bit($number) {
  if ($number < 0) {
  $panic('Input must be a non-negative integer');
}
  if ($number == 0) {
  return -1;
}
  $n = $number;
  $index = 0;
  while ($n % 2 == 0) {
  $n = _intdiv($n, 2);
  $index = $index + 1;
};
  return $index;
}
echo rtrim(_str(index_of_rightmost_set_bit(0))), PHP_EOL;
echo rtrim(_str(index_of_rightmost_set_bit(5))), PHP_EOL;
echo rtrim(_str(index_of_rightmost_set_bit(36))), PHP_EOL;
echo rtrim(_str(index_of_rightmost_set_bit(8))), PHP_EOL;
