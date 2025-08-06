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
function subarray($xs, $start, $end) {
  $result = [];
  $k = $start;
  while ($k < $end) {
  $result = _append($result, $xs[$k]);
  $k = $k + 1;
};
  return $result;
}
function merge($left_half, $right_half) {
  $result = [];
  $i = 0;
  $j = 0;
  while ($i < count($left_half) && $j < count($right_half)) {
  if ($left_half[$i] < $right_half[$j]) {
  $result = _append($result, $left_half[$i]);
  $i = $i + 1;
} else {
  $result = _append($result, $right_half[$j]);
  $j = $j + 1;
}
};
  while ($i < count($left_half)) {
  $result = _append($result, $left_half[$i]);
  $i = $i + 1;
};
  while ($j < count($right_half)) {
  $result = _append($result, $right_half[$j]);
  $j = $j + 1;
};
  return $result;
}
function merge_sort($array) {
  if (count($array) <= 1) {
  return $array;
}
  $middle = count($array) / 2;
  $left_half = subarray($array, 0, $middle);
  $right_half = subarray($array, $middle, count($array));
  $sorted_left = merge_sort($left_half);
  $sorted_right = merge_sort($right_half);
  return merge($sorted_left, $sorted_right);
}
echo rtrim(_str(merge_sort([5, 3, 1, 4, 2]))), PHP_EOL;
echo rtrim(_str(merge_sort([-2, 3, -10, 11, 99, 100000, 100, -200]))), PHP_EOL;
echo rtrim(_str(merge_sort([-200]))), PHP_EOL;
echo rtrim(_str(merge_sort([]))), PHP_EOL;
