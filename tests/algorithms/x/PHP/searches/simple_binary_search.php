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
function binary_search($arr, $item) {
  global $arr1, $arr2, $arr3, $arr4, $arr5;
  $low = 0;
  $high = _isub(count($arr), 1);
  while ($low <= $high) {
  $mid = _intdiv((_iadd($low, $high)), 2);
  $val = $arr[$mid];
  if ($val == $item) {
  return true;
}
  if ($item < $val) {
  $high = _isub($mid, 1);
} else {
  $low = _iadd($mid, 1);
}
};
  return false;
}
$arr1 = [0, 1, 2, 8, 13, 17, 19, 32, 42];
echo rtrim(json_encode(binary_search($arr1, 3), 1344)), PHP_EOL;
echo rtrim(json_encode(binary_search($arr1, 13), 1344)), PHP_EOL;
$arr2 = [4, 4, 5, 6, 7];
echo rtrim(json_encode(binary_search($arr2, 4), 1344)), PHP_EOL;
echo rtrim(json_encode(binary_search($arr2, -10), 1344)), PHP_EOL;
$arr3 = [-18, 2];
echo rtrim(json_encode(binary_search($arr3, -18), 1344)), PHP_EOL;
$arr4 = [5];
echo rtrim(json_encode(binary_search($arr4, 5), 1344)), PHP_EOL;
$arr5 = [];
echo rtrim(json_encode(binary_search($arr5, 1), 1344)), PHP_EOL;
