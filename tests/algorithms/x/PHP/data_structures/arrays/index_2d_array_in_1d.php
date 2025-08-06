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
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function iterator_values($matrix) {
  $result = [];
  foreach ($matrix as $row) {
  foreach ($row as $value) {
  $result = _append($result, $value);
};
};
  return $result;
}
function index_2d_array_in_1d($array, $index) {
  $rows = count($array);
  $cols = count($array[0]);
  if ($rows == 0 || $cols == 0) {
  $panic('no items in array');
}
  if ($index < 0 || $index >= $rows * $cols) {
  $panic('index out of range');
}
  return $array[intval(_intdiv($index, $cols))][$index % $cols];
}
echo rtrim(_str(iterator_values([[5], [-523], [-1], [34], [0]]))), PHP_EOL;
echo rtrim(_str(iterator_values([[5, -523, -1], [34, 0]]))), PHP_EOL;
echo rtrim(_str(index_2d_array_in_1d([[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]], 5))), PHP_EOL;
