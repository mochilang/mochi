<?php
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function is_sorted($xs) {
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[_isub($i, 1)] > $xs[$i]) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function exponential_search($arr, $item) {
  if (!is_sorted($arr)) {
  _panic('sorted_collection must be sorted in ascending order');
}
  if (count($arr) == 0) {
  return -1;
}
  if ($arr[0] == $item) {
  return 0;
}
  $bound = 1;
  while ($bound < count($arr) && $arr[$bound] < $item) {
  $bound = _imul($bound, 2);
};
  $left = _intdiv($bound, 2);
  $right = $bound;
  if ($right >= count($arr)) {
  $right = _isub(count($arr), 1);
}
  while ($left <= $right) {
  $mid = _iadd($left, _intdiv((_isub($right, $left)), 2));
  if ($arr[$mid] == $item) {
  return $mid;
}
  if ($arr[$mid] > $item) {
  $right = _isub($mid, 1);
} else {
  $left = _iadd($mid, 1);
}
};
  return -1;
}
