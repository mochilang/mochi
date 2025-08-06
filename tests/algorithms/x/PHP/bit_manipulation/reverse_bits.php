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
function get_reverse_bit_string($number) {
  $bit_string = '';
  $n = $number;
  $i = 0;
  while ($i < 32) {
  $bit_string = $bit_string . _str($n % 2);
  $n = _intdiv($n, 2);
  $i = $i + 1;
};
  return $bit_string;
}
function reverse_bit($number) {
  if ($number < 0) {
  $panic('the value of input must be positive');
}
  $n = $number;
  $result = 0;
  $i = 1;
  while ($i <= 32) {
  $result = $result * 2;
  $end_bit = $n % 2;
  $n = _intdiv($n, 2);
  $result = $result + $end_bit;
  $i = $i + 1;
};
  return get_reverse_bit_string($result);
}
echo rtrim(reverse_bit(25)), PHP_EOL;
echo rtrim(reverse_bit(37)), PHP_EOL;
echo rtrim(reverse_bit(21)), PHP_EOL;
echo rtrim(reverse_bit(58)), PHP_EOL;
echo rtrim(reverse_bit(0)), PHP_EOL;
echo rtrim(reverse_bit(256)), PHP_EOL;
