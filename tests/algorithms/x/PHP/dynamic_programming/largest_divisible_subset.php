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
function sort_list($nums) {
  $arr = $nums;
  $i = 1;
  while ($i < count($arr)) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0 && $arr[$j] > $key) {
  $arr[$j + 1] = $arr[$j];
  $j = $j - 1;
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
}
function largest_divisible_subset($items) {
  if (count($items) == 0) {
  return [];
}
  $nums = sort_list($items);
  $n = count($nums);
  $memo = [];
  $prev = [];
  $i = 0;
  while ($i < $n) {
  $memo = _append($memo, 1);
  $prev = _append($prev, $i);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $i) {
  if (($nums[$j] == 0 || fmod($nums[$i], $nums[$j]) == 0) && $memo[$j] + 1 > $memo[$i]) {
  $memo[$i] = $memo[$j] + 1;
  $prev[$i] = $j;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $ans = 0 - 1;
  $last_index = 0 - 1;
  $i = 0;
  while ($i < $n) {
  if ($memo[$i] > $ans) {
  $ans = $memo[$i];
  $last_index = $i;
}
  $i = $i + 1;
};
  if ($last_index == 0 - 1) {
  return [];
}
  $result = [$nums[$last_index]];
  while ($prev[$last_index] != $last_index) {
  $last_index = $prev[$last_index];
  $result = _append($result, $nums[$last_index]);
};
  return $result;
}
function main() {
  $items = [1, 16, 7, 8, 4];
  $subset = largest_divisible_subset($items);
  echo rtrim('The longest divisible subset of ' . _str($items) . ' is ' . _str($subset) . '.'), PHP_EOL;
}
main();
