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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function list_min($xs) {
  $i = 1;
  $m = $xs[0];
  while ($i < count($xs)) {
  if ($xs[$i] < $m) {
  $m = $xs[$i];
}
  $i = _iadd($i, 1);
};
  return $m;
}
function list_max($xs) {
  $i = 1;
  $m = $xs[0];
  while ($i < count($xs)) {
  if ($xs[$i] > $m) {
  $m = $xs[$i];
}
  $i = _iadd($i, 1);
};
  return $m;
}
function remove_once($xs, $value) {
  $res = [];
  $removed = false;
  $i = 0;
  while ($i < count($xs)) {
  if (!$removed && $xs[$i] == $value) {
  $removed = true;
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function reverse_list($xs) {
  $res = [];
  $i = _isub(count($xs), 1);
  while ($i >= 0) {
  $res = _append($res, $xs[$i]);
  $i = _isub($i, 1);
};
  return $res;
}
function merge_sort($collection) {
  $start = [];
  $end = [];
  $coll = $collection;
  while (count($coll) > 1) {
  $mn = list_min($coll);
  $mx = list_max($coll);
  $start = _append($start, $mn);
  $end = _append($end, $mx);
  $coll = remove_once($coll, $mn);
  $coll = remove_once($coll, $mx);
};
  $end = reverse_list($end);
  return array_merge(array_merge($start, $coll), $end);
}
function test_merge_sort() {
  if (merge_sort([0, 5, 3, 2, 2]) != [0, 2, 2, 3, 5]) {
  _panic('case1 failed');
}
  if (merge_sort([]) != []) {
  _panic('case2 failed');
}
  if (merge_sort([-2, -5, -45]) != [-45, -5, -2]) {
  _panic('case3 failed');
}
}
function main() {
  test_merge_sort();
  echo rtrim(_str(merge_sort([0, 5, 3, 2, 2]))), PHP_EOL;
}
main();
