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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function factorial($n) {
  $result = 1;
  $i = 2;
  while ($i <= $n) {
  $result = _imul($result, $i);
  $i = _iadd($i, 1);
};
  return $result;
}
function nth_permutation($digits, $index) {
  $chars = $digits;
  $n = $index;
  $res = '';
  $k = strlen($chars);
  while ($k > 0) {
  $f = factorial(_isub($k, 1));
  $pos = _intdiv($n, $f);
  $n = _imod($n, $f);
  $res = $res . substr($chars, $pos, _iadd($pos, 1) - $pos);
  $chars = substr($chars, 0, $pos) . substr($chars, _iadd($pos, 1), strlen($chars) - _iadd($pos, 1));
  $k = _isub($k, 1);
};
  return $res;
}
function solution() {
  return nth_permutation('0123456789', 999999);
}
function main() {
  echo rtrim(solution()), PHP_EOL;
}
main();
