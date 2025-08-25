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
function sum_of_series($first_term, $common_diff, $num_of_terms) {
  $total = _intdiv($num_of_terms * (2 * $first_term + ($num_of_terms - 1) * $common_diff), 2);
  return $total;
}
function test_sum_of_series() {
  if (sum_of_series(1, 1, 10) != 55) {
  _panic('sum_of_series(1, 1, 10) failed');
}
  if (sum_of_series(1, 10, 100) != 49600) {
  _panic('sum_of_series(1, 10, 100) failed');
}
}
function main() {
  test_sum_of_series();
  echo rtrim(json_encode(sum_of_series(1, 1, 10), 1344)), PHP_EOL;
}
main();
