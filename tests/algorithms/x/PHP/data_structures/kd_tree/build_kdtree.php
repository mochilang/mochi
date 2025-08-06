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
$tree = [];
function sort_points($points, $axis) {
  global $tree, $pts, $root;
  $arr = $points;
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr) - 1) {
  if ($arr[$j][$axis] > $arr[$j + 1][$axis]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function build_kdtree($points, $depth) {
  global $tree, $pts, $root;
  if (count($points) == 0) {
  return 0 - 1;
}
  $k = count($points[0]);
  $axis = $depth % $k;
  $sorted = sort_points($points, $axis);
  $median_idx = count($sorted) / 2;
  $left_points = array_slice($sorted, 0, $median_idx - 0);
  $right_points = array_slice($sorted, $median_idx + 1, count($sorted) - ($median_idx + 1));
  $idx = count($tree);
  $tree = _append($tree, ['point' => $sorted[$median_idx], 'left' => 0 - 1, 'right' => 0 - 1]);
  $left_idx = build_kdtree($left_points, $depth + 1);
  $right_idx = build_kdtree($right_points, $depth + 1);
  $node = $tree[$idx];
  $node['left'] = $left_idx;
  $node['right'] = $right_idx;
  $tree[$idx] = $node;
  return $idx;
}
$pts = [[2.0, 3.0], [5.0, 4.0], [9.0, 6.0], [4.0, 7.0], [8.0, 1.0], [7.0, 2.0]];
$root = build_kdtree($pts, 0);
echo rtrim(_str($tree)), PHP_EOL;
echo rtrim(json_encode($root, 1344)), PHP_EOL;
