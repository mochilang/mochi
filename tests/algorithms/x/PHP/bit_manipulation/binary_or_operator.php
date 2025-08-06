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
function binary_or($a, $b) {
  if ($a < 0 || $b < 0) {
  return 'ValueError';
}
  $res = '';
  $x = $a;
  $y = $b;
  while ($x > 0 || $y > 0) {
  $bit_a = $x % 2;
  $bit_b = $y % 2;
  if ($bit_a == 1 || $bit_b == 1) {
  $res = '1' . $res;
} else {
  $res = '0' . $res;
}
  $x = _intdiv($x, 2);
  $y = _intdiv($y, 2);
};
  if ($res == '') {
  $res = '0';
}
  return '0b' . $res;
}
echo rtrim(binary_or(25, 32)), PHP_EOL;
echo rtrim(binary_or(37, 50)), PHP_EOL;
echo rtrim(binary_or(21, 30)), PHP_EOL;
echo rtrim(binary_or(58, 73)), PHP_EOL;
echo rtrim(binary_or(0, 255)), PHP_EOL;
echo rtrim(binary_or(0, 256)), PHP_EOL;
