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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function largest_prime_factor($n) {
  if ($n <= 0) {
  _panic('Parameter n must be greater than or equal to one.');
}
  $num = $n;
  $prime = 1;
  $i = 2;
  while ($i * $i <= $num) {
  while ($num % $i == 0) {
  $prime = $i;
  $num = _intdiv($num, $i);
};
  $i = $i + 1;
};
  if ($num > 1) {
  $prime = $num;
}
  return $prime;
}
function main() {
  echo rtrim(_str(largest_prime_factor(600851475143))), PHP_EOL;
}
main();
