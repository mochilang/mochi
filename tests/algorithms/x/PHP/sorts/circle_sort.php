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
function circle_sort_util(&$collection, $low, $high) {
  $swapped = false;
  if ($low == $high) {
  return $swapped;
}
  $left = $low;
  $right = $high;
  while ($left < $right) {
  if ($collection[$left] > $collection[$right]) {
  $tmp = $collection[$left];
  $collection[$left] = $collection[$right];
  $collection[$right] = $tmp;
  $swapped = true;
}
  $left = _iadd($left, 1);
  $right = _isub($right, 1);
};
  if ($left == $right && $collection[$left] > $collection[_iadd($right, 1)]) {
  $tmp2 = $collection[$left];
  $collection[$left] = $collection[_iadd($right, 1)];
  $collection[_iadd($right, 1)] = $tmp2;
  $swapped = true;
}
  $mid = _iadd($low, _intdiv((_isub($high, $low)), 2));
  $left_swap = circle_sort_util($collection, $low, $mid);
  $right_swap = circle_sort_util($collection, _iadd($mid, 1), $high);
  if ($swapped || $left_swap || $right_swap) {
  return true;
} else {
  return false;
}
}
function circle_sort($collection) {
  if (count($collection) < 2) {
  return $collection;
}
  $is_not_sorted = true;
  while ($is_not_sorted) {
  $is_not_sorted = circle_sort_util($collection, 0, _isub(count($collection), 1));
};
  return $collection;
}
echo rtrim(_str(circle_sort([0, 5, 3, 2, 2]))), PHP_EOL;
echo rtrim(_str(circle_sort([]))), PHP_EOL;
echo rtrim(_str(circle_sort([-2, 5, 0, -45]))), PHP_EOL;
