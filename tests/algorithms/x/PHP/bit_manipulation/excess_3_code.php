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
function excess_3_code($number) {
  $n = $number;
  if ($n < 0) {
  $n = 0;
}
  $mapping = ['0011', '0100', '0101', '0110', '0111', '1000', '1001', '1010', '1011', '1100'];
  $res = '';
  if ($n == 0) {
  $res = $mapping[0];
} else {
  while ($n > 0) {
  $digit = $n % 10;
  $res = $mapping[$digit] . $res;
  $n = _intdiv($n, 10);
};
}
  return '0b' . $res;
}
function main() {
  echo rtrim(excess_3_code(0)), PHP_EOL;
  echo rtrim(excess_3_code(3)), PHP_EOL;
  echo rtrim(excess_3_code(2)), PHP_EOL;
  echo rtrim(excess_3_code(20)), PHP_EOL;
  echo rtrim(excess_3_code(120)), PHP_EOL;
}
main();
