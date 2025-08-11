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
function interpolation_search($arr, $item) {
  $left = 0;
  $right = _isub(count($arr), 1);
  while ($left <= $right) {
  if ($arr[$left] == $arr[$right]) {
  if ($arr[$left] == $item) {
  return $left;
};
  return -1;
}
  $point = _iadd($left, _intdiv((_imul((_isub($item, $arr[$left])), (_isub($right, $left)))), (_isub($arr[$right], $arr[$left]))));
  if ($point < 0 || $point >= count($arr)) {
  return -1;
}
  $current = $arr[$point];
  if ($current == $item) {
  return $point;
}
  if ($point < $left) {
  $right = $left;
  $left = $point;
} else {
  if ($point > $right) {
  $left = $right;
  $right = $point;
} else {
  if ($item < $current) {
  $right = _isub($point, 1);
} else {
  $left = _iadd($point, 1);
};
};
}
};
  return -1;
}
function interpolation_search_recursive($arr, $item, $left, $right) {
  if ($left > $right) {
  return -1;
}
  if ($arr[$left] == $arr[$right]) {
  if ($arr[$left] == $item) {
  return $left;
};
  return -1;
}
  $point = _iadd($left, _intdiv((_imul((_isub($item, $arr[$left])), (_isub($right, $left)))), (_isub($arr[$right], $arr[$left]))));
  if ($point < 0 || $point >= count($arr)) {
  return -1;
}
  if ($arr[$point] == $item) {
  return $point;
}
  if ($point < $left) {
  return interpolation_search_recursive($arr, $item, $point, $left);
}
  if ($point > $right) {
  return interpolation_search_recursive($arr, $item, $right, $left);
}
  if ($arr[$point] > $item) {
  return interpolation_search_recursive($arr, $item, $left, _isub($point, 1));
}
  return interpolation_search_recursive($arr, $item, _iadd($point, 1), $right);
}
function interpolation_search_by_recursion($arr, $item) {
  return interpolation_search_recursive($arr, $item, 0, _isub(count($arr), 1));
}
echo rtrim(_str(interpolation_search([1, 2, 3, 4, 5], 2))), PHP_EOL;
echo rtrim(_str(interpolation_search([1, 2, 3, 4, 5], 6))), PHP_EOL;
echo rtrim(_str(interpolation_search_by_recursion([0, 5, 7, 10, 15], 5))), PHP_EOL;
echo rtrim(_str(interpolation_search_by_recursion([0, 5, 7, 10, 15], 100))), PHP_EOL;
echo rtrim(_str(interpolation_search_by_recursion([5, 5, 5, 5, 5], 3))), PHP_EOL;
