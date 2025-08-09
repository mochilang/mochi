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
function int_sqrt($n) {
  $r = 0;
  while (_imul((_iadd($r, 1)), (_iadd($r, 1))) <= $n) {
  $r = _iadd($r, 1);
};
  return $r;
}
function sum_of_divisors($n) {
  $total = 0;
  $root = int_sqrt($n);
  $i = 1;
  while ($i <= $root) {
  if (_imod($n, $i) == 0) {
  if (_imul($i, $i) == $n) {
  $total = _iadd($total, $i);
} else {
  $total = _iadd(_iadd($total, $i), (_intdiv($n, $i)));
};
}
  $i = _iadd($i, 1);
};
  return _isub($total, $n);
}
function solution($limit) {
  $total = 0;
  $i = 1;
  while ($i < $limit) {
  $s = sum_of_divisors($i);
  if ($s != $i && sum_of_divisors($s) == $i) {
  $total = _iadd($total, $i);
}
  $i = _iadd($i, 1);
};
  return $total;
}
echo rtrim(json_encode(solution(10000), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(5000), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(1000), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(100), 1344)), PHP_EOL;
echo rtrim(json_encode(solution(50), 1344)), PHP_EOL;
