<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function toOct($n) {
  if ($n == 0) {
  return '0';
}
  $digits = '01234567';
  $out = '';
  $v = $n;
  while ($v > 0) {
  $d = $v % 8;
  $out = substr($digits, $d, $d + 1 - $d) . $out;
  $v = _intdiv($v, 8);
};
  return $out;
}
function main() {
  $i = 0;
  while (true) {
  echo rtrim(toOct($i)), PHP_EOL;
  $i = $i + 1;
};
}
main();
