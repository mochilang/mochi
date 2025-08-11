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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
function merge($a, $low, $mid, $high) {
  $left = array_slice($a, $low, $mid - $low);
  $right = array_slice($a, $mid, _iadd($high, 1) - $mid);
  $result = [];
  while (count($left) > 0 && count($right) > 0) {
  if ($left[0] <= $right[0]) {
  $result = _append($result, $left[0]);
  $left = array_slice($left, 1);
} else {
  $result = _append($result, $right[0]);
  $right = array_slice($right, 1);
}
};
  $i = 0;
  while ($i < count($left)) {
  $result = _append($result, $left[$i]);
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < count($right)) {
  $result = _append($result, $right[$i]);
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < count($result)) {
  $a[_iadd($low, $i)] = $result[$i];
  $i = _iadd($i, 1);
};
  return $a;
}
function iter_merge_sort($items) {
  $n = count($items);
  if ($n <= 1) {
  return $items;
}
  $arr = array_slice($items, 0);
  $p = 2;
  while ($p <= $n) {
  $i = 0;
  while ($i < $n) {
  $high = _isub(_iadd($i, $p), 1);
  if ($high >= $n) {
  $high = _isub($n, 1);
}
  $low = $i;
  $mid = _intdiv((_iadd(_iadd($low, $high), 1)), 2);
  $arr = merge($arr, $low, $mid, $high);
  $i = _iadd($i, $p);
};
  if (_imul($p, 2) >= $n) {
  $mid2 = _isub($i, $p);
  $arr = merge($arr, 0, $mid2, _isub($n, 1));
  break;
}
  $p = _imul($p, 2);
};
  return $arr;
}
function list_to_string($arr) {
  $s = '[';
  $i = 0;
  while ($i < count($arr)) {
  $s = $s . _str($arr[$i]);
  if ($i < _isub(count($arr), 1)) {
  $s = $s . ', ';
}
  $i = _iadd($i, 1);
};
  return $s . ']';
}
echo rtrim(list_to_string(iter_merge_sort([5, 9, 8, 7, 1, 2, 7]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([1]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([2, 1]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([4, 3, 2, 1]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([5, 4, 3, 2, 1]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([-2, -9, -1, -4]))), PHP_EOL;
echo rtrim(list_to_string(iter_merge_sort([]))), PHP_EOL;
