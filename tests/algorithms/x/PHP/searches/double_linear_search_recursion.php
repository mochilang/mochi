<?php
ini_set('memory_limit', '-1');
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
function search($list_data, $key, $left, $right) {
  $r = $right;
  if ($r == 0) {
  $r = _isub(count($list_data), 1);
}
  if ($left > $r) {
  return -1;
} else {
  if ($list_data[$left] == $key) {
  return $left;
} else {
  if ($list_data[$r] == $key) {
  return $r;
} else {
  return search($list_data, $key, _iadd($left, 1), _isub($r, 1));
};
};
}
}
function main() {
  echo rtrim(json_encode(search([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 5, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([1, 2, 4, 5, 3], 4, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([1, 2, 4, 5, 3], 6, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([5], 5, 0, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(search([], 1, 0, 0), 1344)), PHP_EOL;
}
main();
