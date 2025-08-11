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
function swap($xs, $i, $j) {
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  if ($k == $i) {
  $res = _append($res, $xs[$j]);
} else {
  if ($k == $j) {
  $res = _append($res, $xs[$i]);
} else {
  $res = _append($res, $xs[$k]);
};
}
  $k = _iadd($k, 1);
};
  return $res;
}
function wiggle_sort($nums) {
  $i = 0;
  $res = $nums;
  while ($i < count($res)) {
  $j = ($i == 0 ? _isub(count($res), 1) : _isub($i, 1));
  $prev = $res[$j];
  $curr = $res[$i];
  if ((_imod($i, 2) == 1) == ($prev > $curr)) {
  $res = swap($res, $j, $i);
}
  $i = _iadd($i, 1);
};
  return $res;
}
echo rtrim(_str(wiggle_sort([3.0, 5.0, 2.0, 1.0, 6.0, 4.0]))), PHP_EOL;
echo rtrim(_str(wiggle_sort([0.0, 5.0, 3.0, 2.0, 2.0]))), PHP_EOL;
echo rtrim(_str(wiggle_sort([]))), PHP_EOL;
echo rtrim(_str(wiggle_sort([-2.0, -5.0, -45.0]))), PHP_EOL;
echo rtrim(_str(wiggle_sort([-2.1, -5.68, -45.11]))), PHP_EOL;
