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
function repeat_char($ch, $times) {
  $res = '';
  $i = 0;
  while ($i < $times) {
  $res = $res . $ch;
  $i = $i + 1;
};
  return $res;
}
function to_binary($n) {
  if ($n == 0) {
  return '0';
}
  $res = '';
  $v = $n;
  while ($v > 0) {
  $res = _str($v % 2) . $res;
  $v = _intdiv($v, 2);
};
  return $res;
}
function pow2($exp) {
  $res = 1;
  $i = 0;
  while ($i < $exp) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
}
function twos_complement($number) {
  if ($number > 0) {
  $panic('input must be a negative integer');
}
  if ($number == 0) {
  return '0b0';
}
  $abs_number = ($number < 0 ? -$number : $number);
  $binary_number_length = strlen(to_binary($abs_number));
  $complement_value = pow2($binary_number_length) - $abs_number;
  $complement_binary = to_binary($complement_value);
  $padding = repeat_char('0', $binary_number_length - strlen($complement_binary));
  $twos_complement_number = '1' . $padding . $complement_binary;
  return '0b' . $twos_complement_number;
}
echo rtrim(twos_complement(0)), PHP_EOL;
echo rtrim(twos_complement(-1)), PHP_EOL;
echo rtrim(twos_complement(-5)), PHP_EOL;
echo rtrim(twos_complement(-17)), PHP_EOL;
echo rtrim(twos_complement(-207)), PHP_EOL;
