<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
function is_happy_number($num) {
  if ($num <= 0) {
  _panic('num must be a positive integer');
}
  $seen = [];
  $n = $num;
  while ($n != 1) {
  $i = 0;
  while ($i < count($seen)) {
  if ($seen[$i] == $n) {
  return false;
}
  $i = $i + 1;
};
  $seen = _append($seen, $n);
  $total = 0;
  $temp = $n;
  while ($temp > 0) {
  $digit = $temp % 10;
  $total = $total + $digit * $digit;
  $temp = _intdiv($temp, 10);
};
  $n = $total;
};
  return true;
}
function test_is_happy_number() {
  if (!is_happy_number(19)) {
  _panic('19 should be happy');
}
  if (is_happy_number(2)) {
  _panic('2 should be unhappy');
}
  if (!is_happy_number(23)) {
  _panic('23 should be happy');
}
  if (!is_happy_number(1)) {
  _panic('1 should be happy');
}
}
function main() {
  test_is_happy_number();
  echo rtrim(json_encode(is_happy_number(19), 1344)), PHP_EOL;
}
main();
