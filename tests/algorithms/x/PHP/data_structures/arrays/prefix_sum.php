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
function make_prefix_sum($arr) {
  global $ps, $ps2;
  $prefix = [];
  $running = 0;
  $i = 0;
  while ($i < count($arr)) {
  $running = $running + $arr[$i];
  $prefix = _append($prefix, $running);
  $i = $i + 1;
};
  return ['prefix_sum' => $prefix];
}
function get_sum($ps, $start, $end) {
  global $ps2;
  $prefix = $ps['prefix_sum'];
  if (count($prefix) == 0) {
  $panic('The array is empty.');
}
  if ($start < 0 || $end >= count($prefix) || $start > $end) {
  $panic('Invalid range specified.');
}
  if ($start == 0) {
  return $prefix[$end];
}
  return $prefix[$end] - $prefix[$start - 1];
}
function contains_sum($ps, $target_sum) {
  global $ps2;
  $prefix = $ps['prefix_sum'];
  $sums = [0];
  $i = 0;
  while ($i < count($prefix)) {
  $sum_item = $prefix[$i];
  $j = 0;
  while ($j < count($sums)) {
  if ($sums[$j] == $sum_item - $target_sum) {
  return true;
}
  $j = $j + 1;
};
  $sums = _append($sums, $sum_item);
  $i = $i + 1;
};
  return false;
}
$ps = make_prefix_sum([1, 2, 3]);
echo rtrim(_str(get_sum($ps, 0, 2))), PHP_EOL;
echo rtrim(_str(get_sum($ps, 1, 2))), PHP_EOL;
echo rtrim(_str(get_sum($ps, 2, 2))), PHP_EOL;
echo rtrim(_str(contains_sum($ps, 6))), PHP_EOL;
echo rtrim(_str(contains_sum($ps, 5))), PHP_EOL;
echo rtrim(_str(contains_sum($ps, 3))), PHP_EOL;
echo rtrim(_str(contains_sum($ps, 4))), PHP_EOL;
echo rtrim(_str(contains_sum($ps, 7))), PHP_EOL;
$ps2 = make_prefix_sum([1, -2, 3]);
echo rtrim(_str(contains_sum($ps2, 2))), PHP_EOL;
