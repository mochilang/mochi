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
$nodes = [];
function make_list($length, $value) {
  global $nodes, $test_array, $root;
  $lst = [];
  $i = 0;
  while ($i < $length) {
  $lst = _append($lst, $value);
  $i = $i + 1;
};
  return $lst;
}
function min_list($arr) {
  global $nodes, $test_array, $root;
  $m = $arr[0];
  $i = 1;
  while ($i < count($arr)) {
  if ($arr[$i] < $m) {
  $m = $arr[$i];
}
  $i = $i + 1;
};
  return $m;
}
function max_list($arr) {
  global $nodes, $test_array, $root;
  $m = $arr[0];
  $i = 1;
  while ($i < count($arr)) {
  if ($arr[$i] > $m) {
  $m = $arr[$i];
}
  $i = $i + 1;
};
  return $m;
}
function build_tree($arr) {
  global $nodes, $test_array, $root;
  $n = ['minn' => min_list($arr), 'maxx' => max_list($arr), 'map_left' => make_list(count($arr), 0), 'left' => -1, 'right' => -1];
  if ($n['minn'] == $n['maxx']) {
  $nodes = _append($nodes, $n);
  return count($nodes) - 1;
}
  $pivot = ($n['minn'] + $n['maxx']) / 2;
  $left_arr = [];
  $right_arr = [];
  $i = 0;
  while ($i < count($arr)) {
  $num = $arr[$i];
  if ($num <= $pivot) {
  $left_arr = _append($left_arr, $num);
} else {
  $right_arr = _append($right_arr, $num);
}
  $ml = $n['map_left'];
  $ml[$i] = count($left_arr);
  $n['map_left'] = $ml;
  $i = $i + 1;
};
  if (count($left_arr) > 0) {
  $n['left'] = build_tree($left_arr);
}
  if (count($right_arr) > 0) {
  $n['right'] = build_tree($right_arr);
}
  $nodes = _append($nodes, $n);
  return count($nodes) - 1;
}
function rank_till_index($node_idx, $num, $index) {
  global $nodes, $test_array, $root;
  if ($index < 0 || $node_idx < 0) {
  return 0;
}
  $node = $nodes[$node_idx];
  if ($node['minn'] == $node['maxx']) {
  if ($node['minn'] == $num) {
  return $index + 1;
} else {
  return 0;
};
}
  $pivot = ($node['minn'] + $node['maxx']) / 2;
  if ($num <= $pivot) {
  return rank_till_index($node['left'], $num, $node['map_left'][$index] - 1);
} else {
  return rank_till_index($node['right'], $num, $index - $node['map_left'][$index]);
}
}
function rank($node_idx, $num, $start, $end) {
  global $nodes, $test_array, $root;
  if ($start > $end) {
  return 0;
}
  $rank_till_end = rank_till_index($node_idx, $num, $end);
  $rank_before_start = rank_till_index($node_idx, $num, $start - 1);
  return $rank_till_end - $rank_before_start;
}
function quantile($node_idx, $index, $start, $end) {
  global $nodes, $test_array, $root;
  if ($index > ($end - $start) || $start > $end || $node_idx < 0) {
  return -1;
}
  $node = $nodes[$node_idx];
  if ($node['minn'] == $node['maxx']) {
  return $node['minn'];
}
  $left_start = ($start == 0 ? 0 : $node['map_left'][$start - 1]);
  $num_left = $node['map_left'][$end] - $left_start;
  if ($num_left > $index) {
  return quantile($node['left'], $index, $left_start, $node['map_left'][$end] - 1);
} else {
  return quantile($node['right'], $index - $num_left, $start - $left_start, $end - $node['map_left'][$end]);
}
}
function range_counting($node_idx, $start, $end, $start_num, $end_num) {
  global $nodes, $test_array, $root;
  if ($start > $end || $node_idx < 0 || $start_num > $end_num) {
  return 0;
}
  $node = $nodes[$node_idx];
  if ($node['minn'] > $end_num || $node['maxx'] < $start_num) {
  return 0;
}
  if ($start_num <= $node['minn'] && $node['maxx'] <= $end_num) {
  return $end - $start + 1;
}
  $left = range_counting($node['left'], ($start == 0 ? 0 : $node['map_left'][$start - 1]), $node['map_left'][$end] - 1, $start_num, $end_num);
  $right = range_counting($node['right'], $start - (($start == 0 ? 0 : $node['map_left'][$start - 1])), $end - $node['map_left'][$end], $start_num, $end_num);
  return $left + $right;
}
$test_array = [2, 1, 4, 5, 6, 0, 8, 9, 1, 2, 0, 6, 4, 2, 0, 6, 5, 3, 2, 7];
$root = build_tree($test_array);
echo rtrim('rank_till_index 6 at 6 -> ' . _str(rank_till_index($root, 6, 6))), PHP_EOL;
echo rtrim('rank 6 in [3,13] -> ' . _str(rank($root, 6, 3, 13))), PHP_EOL;
echo rtrim('quantile index 2 in [2,5] -> ' . _str(quantile($root, 2, 2, 5))), PHP_EOL;
echo rtrim('range_counting [3,7] in [1,10] -> ' . _str(range_counting($root, 1, 10, 3, 7))), PHP_EOL;
