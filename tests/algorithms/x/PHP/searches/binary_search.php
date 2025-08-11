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
function is_sorted($arr) {
  $i = 1;
  while ($i < count($arr)) {
  if ($arr[_isub($i, 1)] > $arr[$i]) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function binary_search($sorted_collection, $item) {
  if (!is_sorted($sorted_collection)) {
  return -1;
}
  $left = 0;
  $right = _isub(count($sorted_collection), 1);
  while ($left <= $right) {
  $midpoint = _iadd($left, _intdiv((_isub($right, $left)), 2));
  $current_item = $sorted_collection[$midpoint];
  if ($current_item == $item) {
  return $midpoint;
}
  if ($item < $current_item) {
  $right = _isub($midpoint, 1);
} else {
  $left = _iadd($midpoint, 1);
}
};
  return -1;
}
function binary_search_by_recursion($sorted_collection, $item, $left, $right) {
  if ($right < $left) {
  return -1;
}
  $midpoint = _iadd($left, _intdiv((_isub($right, $left)), 2));
  if ($sorted_collection[$midpoint] == $item) {
  return $midpoint;
}
  if ($sorted_collection[$midpoint] > $item) {
  return binary_search_by_recursion($sorted_collection, $item, $left, _isub($midpoint, 1));
}
  return binary_search_by_recursion($sorted_collection, $item, _iadd($midpoint, 1), $right);
}
function exponential_search($sorted_collection, $item) {
  if (!is_sorted($sorted_collection)) {
  return -1;
}
  if (count($sorted_collection) == 0) {
  return -1;
}
  $bound = 1;
  while ($bound < count($sorted_collection) && $sorted_collection[$bound] < $item) {
  $bound = _imul($bound, 2);
};
  $left = _intdiv($bound, 2);
  $right = min([$bound, _isub(count($sorted_collection), 1)]);
  return binary_search_by_recursion($sorted_collection, $item, $left, $right);
}
function main() {
  $data = [0, 5, 7, 10, 15];
  echo rtrim(_str(binary_search($data, 0))), PHP_EOL;
  echo rtrim(_str(binary_search($data, 15))), PHP_EOL;
  echo rtrim(_str(binary_search($data, 5))), PHP_EOL;
  echo rtrim(_str(binary_search($data, 6))), PHP_EOL;
  echo rtrim(_str(binary_search_by_recursion($data, 0, 0, _isub(count($data), 1)))), PHP_EOL;
  echo rtrim(_str(binary_search_by_recursion($data, 15, 0, _isub(count($data), 1)))), PHP_EOL;
  echo rtrim(_str(binary_search_by_recursion($data, 5, 0, _isub(count($data), 1)))), PHP_EOL;
  echo rtrim(_str(binary_search_by_recursion($data, 6, 0, _isub(count($data), 1)))), PHP_EOL;
  echo rtrim(_str(exponential_search($data, 0))), PHP_EOL;
  echo rtrim(_str(exponential_search($data, 15))), PHP_EOL;
  echo rtrim(_str(exponential_search($data, 5))), PHP_EOL;
  echo rtrim(_str(exponential_search($data, 6))), PHP_EOL;
}
main();
