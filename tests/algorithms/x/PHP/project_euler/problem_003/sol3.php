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
function largest_prime_factor($n) {
  if ($n <= 1) {
  return $n;
}
  $i = 2;
  $ans = 0;
  $m = $n;
  if ($m == 2) {
  return 2;
}
  while ($m > 2) {
  while ($m % $i != 0) {
  $i = $i + 1;
};
  $ans = $i;
  while ($m % $i == 0) {
  $m = _intdiv($m, $i);
};
  $i = $i + 1;
};
  return $ans;
}
echo str_replace('    ', '  ', json_encode(largest_prime_factor(13195), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(largest_prime_factor(10), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(largest_prime_factor(17), 128)), PHP_EOL;
echo str_replace('    ', '  ', json_encode(largest_prime_factor(600851475143), 128)), PHP_EOL;
