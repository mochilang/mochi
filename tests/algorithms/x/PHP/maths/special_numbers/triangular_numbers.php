<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
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
function triangular_number($position) {
  if ($position < 0) {
  _panic('position must be non-negative');
}
  return _intdiv($position * ($position + 1), 2);
}
function test_triangular_number() {
  if (triangular_number(1) != 1) {
  _panic('triangular_number(1) failed');
}
  if (triangular_number(3) != 6) {
  _panic('triangular_number(3) failed');
}
}
function main() {
  test_triangular_number();
  echo rtrim(json_encode(triangular_number(10), 1344)), PHP_EOL;
}
main();
