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
function bitwise_xor($a, $b) {
  $result = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $ax = $x % 2;
  $by = $y % 2;
  if (($ax + $by) % 2 == 1) {
  $result = $result + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $result;
}
function bitwise_and($a, $b) {
  $result = 0;
  $bit = 1;
  $x = $a;
  $y = $b;
  while ($x > 0 && $y > 0) {
  if ($x % 2 == 1 && $y % 2 == 1) {
  $result = $result + $bit;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
  $bit = $bit * 2;
};
  return $result;
}
function bitwise_addition_recursive($number, $other_number) {
  if ($number < 0 || $other_number < 0) {
  $panic('Both arguments MUST be non-negative!');
}
  $bitwise_sum = bitwise_xor($number, $other_number);
  $carry = bitwise_and($number, $other_number);
  if ($carry == 0) {
  return $bitwise_sum;
}
  return bitwise_addition_recursive($bitwise_sum, $carry * 2);
}
echo rtrim(_str(bitwise_addition_recursive(4, 5))), PHP_EOL;
echo rtrim(_str(bitwise_addition_recursive(8, 9))), PHP_EOL;
echo rtrim(_str(bitwise_addition_recursive(0, 4))), PHP_EOL;
