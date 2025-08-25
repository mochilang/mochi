<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function abs_int($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
}
function sum_of_digits($n) {
  $m = abs_int($n);
  $res = 0;
  while ($m > 0) {
  $res = $res + ($m % 10);
  $m = _intdiv($m, 10);
};
  return $res;
}
function sum_of_digits_recursion($n) {
  $m = abs_int($n);
  if ($m < 10) {
  return $m;
}
  return ($m % 10) + sum_of_digits_recursion(_intdiv($m, 10));
}
function sum_of_digits_compact($n) {
  $s = _str(abs_int($n));
  $res = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $res = $res + ((ctype_digit($s[$i]) ? intval($s[$i]) : ord($s[$i])));
  $i = $i + 1;
};
  return $res;
}
function test_sum_of_digits() {
  if (sum_of_digits(12345) != 15) {
  _panic('sum_of_digits 12345 failed');
}
  if (sum_of_digits(123) != 6) {
  _panic('sum_of_digits 123 failed');
}
  if (sum_of_digits(-123) != 6) {
  _panic('sum_of_digits -123 failed');
}
  if (sum_of_digits(0) != 0) {
  _panic('sum_of_digits 0 failed');
}
  if (sum_of_digits_recursion(12345) != 15) {
  _panic('recursion 12345 failed');
}
  if (sum_of_digits_recursion(123) != 6) {
  _panic('recursion 123 failed');
}
  if (sum_of_digits_recursion(-123) != 6) {
  _panic('recursion -123 failed');
}
  if (sum_of_digits_recursion(0) != 0) {
  _panic('recursion 0 failed');
}
  if (sum_of_digits_compact(12345) != 15) {
  _panic('compact 12345 failed');
}
  if (sum_of_digits_compact(123) != 6) {
  _panic('compact 123 failed');
}
  if (sum_of_digits_compact(-123) != 6) {
  _panic('compact -123 failed');
}
  if (sum_of_digits_compact(0) != 0) {
  _panic('compact 0 failed');
}
}
function main() {
  test_sum_of_digits();
  echo rtrim(_str(sum_of_digits(12345))), PHP_EOL;
}
main();
