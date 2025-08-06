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
function repeat_char($ch, $count) {
  $res = '';
  $i = 0;
  while ($i < $count) {
  $res = $res . $ch;
  $i = $i + 1;
};
  return $res;
}
function abs_int($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
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
function to_binary_no_prefix($n) {
  $v = $n;
  if ($v < 0) {
  $v = -$v;
}
  if ($v == 0) {
  return '0';
}
  $res = '';
  while ($v > 0) {
  $res = _str($v % 2) . $res;
  $v = _intdiv($v, 2);
};
  return $res;
}
function logical_left_shift($number, $shift_amount) {
  if ($number < 0 || $shift_amount < 0) {
  $panic('both inputs must be positive integers');
}
  $binary_number = '0b' . to_binary_no_prefix($number);
  return $binary_number . repeat_char('0', $shift_amount);
}
function logical_right_shift($number, $shift_amount) {
  if ($number < 0 || $shift_amount < 0) {
  $panic('both inputs must be positive integers');
}
  $binary_number = to_binary_no_prefix($number);
  if ($shift_amount >= strlen($binary_number)) {
  return '0b0';
}
  $shifted = substr($binary_number, 0, strlen($binary_number) - $shift_amount - 0);
  return '0b' . $shifted;
}
function arithmetic_right_shift($number, $shift_amount) {
  $binary_number = '';
  if ($number >= 0) {
  $binary_number = '0' . to_binary_no_prefix($number);
} else {
  $length = strlen(to_binary_no_prefix(-$number));
  $intermediate = abs_int($number) - pow2($length);
  $bin_repr = to_binary_no_prefix($intermediate);
  $binary_number = '1' . repeat_char('0', $length - strlen($bin_repr)) . $bin_repr;
}
  if ($shift_amount >= strlen($binary_number)) {
  $sign = substr($binary_number, 0, 1 - 0);
  return '0b' . repeat_char($sign, strlen($binary_number));
}
  $sign = substr($binary_number, 0, 1 - 0);
  $shifted = substr($binary_number, 0, strlen($binary_number) - $shift_amount - 0);
  return '0b' . repeat_char($sign, $shift_amount) . $shifted;
}
function main() {
  echo rtrim(logical_left_shift(17, 2)), PHP_EOL;
  echo rtrim(logical_right_shift(1983, 4)), PHP_EOL;
  echo rtrim(arithmetic_right_shift(-17, 2)), PHP_EOL;
}
main();
