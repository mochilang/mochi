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
function cocktail_shaker_sort($arr) {
  $start = 0;
  $end = _isub(count($arr), 1);
  while ($start < $end) {
  $swapped = false;
  $i = $start;
  while ($i < $end) {
  if ($arr[$i] > $arr[_iadd($i, 1)]) {
  $temp = $arr[$i];
  $arr[$i] = $arr[_iadd($i, 1)];
  $arr[_iadd($i, 1)] = $temp;
  $swapped = true;
}
  $i = _iadd($i, 1);
};
  if (!$swapped) {
  break;
}
  $end = _isub($end, 1);
  $i = $end;
  while ($i > $start) {
  if ($arr[$i] < $arr[_isub($i, 1)]) {
  $temp2 = $arr[$i];
  $arr[$i] = $arr[_isub($i, 1)];
  $arr[_isub($i, 1)] = $temp2;
  $swapped = true;
}
  $i = _isub($i, 1);
};
  if (!$swapped) {
  break;
}
  $start = _iadd($start, 1);
};
  return $arr;
}
echo rtrim(_str(cocktail_shaker_sort([4, 5, 2, 1, 2]))), PHP_EOL;
echo rtrim(_str(cocktail_shaker_sort([-4, 5, 0, 1, 2, 11]))), PHP_EOL;
echo rtrim(_str(cocktail_shaker_sort([1, 2, 3, 4, 5]))), PHP_EOL;
