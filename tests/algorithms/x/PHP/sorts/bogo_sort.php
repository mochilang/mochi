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
$seed = 1;
function mochi_rand() {
  global $seed, $data;
  $seed = _imod((_iadd(_imul($seed, 1103515245), 12345)), 2147483648);
  return $seed;
}
function rand_range($max) {
  global $seed, $data;
  return _imod(mochi_rand(), $max);
}
function mochi_shuffle($list_int) {
  global $seed, $data;
  $i = _isub(count($list_int), 1);
  while ($i > 0) {
  $j = rand_range(_iadd($i, 1));
  $tmp = $list_int[$i];
  $list_int[$i] = $list_int[$j];
  $list_int[$j] = $tmp;
  $i = _isub($i, 1);
};
  return $list_int;
}
function is_sorted($list_int) {
  global $seed, $data;
  $i = 0;
  while ($i < _isub(count($list_int), 1)) {
  if ($list_int[$i] > $list_int[_iadd($i, 1)]) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function bogo_sort($list_int) {
  global $seed, $data;
  $res = $list_int;
  while (!is_sorted($res)) {
  $res = mochi_shuffle($res);
};
  return $res;
}
$data = [3, 2, 1];
echo rtrim(_str(bogo_sort($data))), PHP_EOL;
