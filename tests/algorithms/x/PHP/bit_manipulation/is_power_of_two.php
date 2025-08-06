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
function is_power_of_two($number) {
  if ($number < 0) {
  $panic('number must not be negative');
}
  $n = $number;
  if ($n == 0) {
  return true;
}
  while ($n % 2 == 0) {
  $n = _intdiv($n, 2);
};
  return $n == 1;
}
