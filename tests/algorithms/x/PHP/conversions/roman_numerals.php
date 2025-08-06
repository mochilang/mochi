<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$roman_values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
$roman_symbols = ['M', 'CM', 'D', 'CD', 'C', 'XC', 'L', 'XL', 'X', 'IX', 'V', 'IV', 'I'];
function char_value($c) {
  global $roman_values, $roman_symbols;
  if ($c == 'I') {
  return 1;
}
  if ($c == 'V') {
  return 5;
}
  if ($c == 'X') {
  return 10;
}
  if ($c == 'L') {
  return 50;
}
  if ($c == 'C') {
  return 100;
}
  if ($c == 'D') {
  return 500;
}
  if ($c == 'M') {
  return 1000;
}
  return 0;
}
function roman_to_int($roman) {
  global $roman_values, $roman_symbols;
  $total = 0;
  $i = 0;
  while ($i < strlen($roman)) {
  if ($i + 1 < strlen($roman) && char_value(substr($roman, $i, $i + 1 - $i)) < char_value(substr($roman, $i + 1, $i + 1 + 1 - ($i + 1)))) {
  $total = $total + char_value(substr($roman, $i + 1, $i + 1 + 1 - ($i + 1))) - char_value(substr($roman, $i, $i + 1 - $i));
  $i = $i + 2;
} else {
  $total = $total + char_value(substr($roman, $i, $i + 1 - $i));
  $i = $i + 1;
}
};
  return $total;
}
function int_to_roman($number) {
  global $roman_values, $roman_symbols;
  $num = $number;
  $res = '';
  $i = 0;
  while ($i < count($roman_values)) {
  $value = $roman_values[$i];
  $symbol = $roman_symbols[$i];
  $factor = _intdiv($num, $value);
  $num = $num % $value;
  $j = 0;
  while ($j < $factor) {
  $res = $res . $symbol;
  $j = $j + 1;
};
  if ($num == 0) {
  break;
}
  $i = $i + 1;
};
  return $res;
}
