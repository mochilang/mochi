<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
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
$seed = 1;
function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function random() {
  global $seed;
  return (1.0 * mochi_rand()) / 2147483648.0;
}
function hypercube_points($num_points, $cube_size, $num_dimensions) {
  global $seed;
  $pts = [];
  $i = 0;
  while ($i < $num_points) {
  $p = [];
  $j = 0;
  while ($j < $num_dimensions) {
  $p = _append($p, $cube_size * random());
  $j = $j + 1;
};
  $pts = _append($pts, $p);
  $i = $i + 1;
};
  return $pts;
}
function sort_points(&$points, $axis) {
  global $seed;
  $n = count($points);
  $i = 1;
  while ($i < $n) {
  $key = $points[$i];
  $j = $i - 1;
  while ($j >= 0 && $points[$j][$axis] > $key[$axis]) {
  $points[$j + 1] = $points[$j];
  $j = $j - 1;
};
  $points[$j + 1] = $key;
  $i = $i + 1;
};
  return $points;
}
function sublist($arr, $start, $end) {
  global $seed;
  $res = [];
  $i = $start;
  while ($i < $end) {
  $res = _append($res, $arr[$i]);
  $i = $i + 1;
};
  return $res;
}
function shift_nodes(&$nodes, $offset) {
  global $seed;
  $i = 0;
  while ($i < count($nodes)) {
  if ($nodes[$i]['left'] != 0 - 1) {
  $nodes[$i]['left'] = $nodes[$i]['left'] + $offset;
}
  if ($nodes[$i]['right'] != 0 - 1) {
  $nodes[$i]['right'] = $nodes[$i]['right'] + $offset;
}
  $i = $i + 1;
};
  return $nodes;
}
function build_kdtree($points, $depth) {
  global $seed;
  if (count($points) == 0) {
  return ['index' => 0 - 1, 'nodes' => []];
}
  $k = count($points[0]);
  $axis = $depth % $k;
  $points = sort_points($points, $axis);
  $median = count($points) / 2;
  $left_points = sublist($points, 0, $median);
  $right_points = sublist($points, $median + 1, count($points));
  $left_res = build_kdtree($left_points, $depth + 1);
  $right_res = build_kdtree($right_points, $depth + 1);
  $offset = _len($left_res['nodes']) + 1;
  $shifted_right = shift_nodes($right_res['nodes'], $offset);
  $nodes = $left_res['nodes'];
  $left_index = $left_res['index'];
  $right_index = ($right_res['index'] == 0 - 1 ? 0 - 1 : $right_res['index'] + $offset);
  $nodes = _append($nodes, ['point' => $points[$median], 'left' => $left_index, 'right' => $right_index]);
  $nodes = array_merge($nodes, $shifted_right);
  $root_index = _len($left_res['nodes']);
  return ['index' => $root_index, 'nodes' => $nodes];
}
function square_distance($a, $b) {
  global $seed;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  return $sum;
}
function nearest_neighbour_search($tree, $root, $query_point) {
  global $seed;
  $nearest_point = [];
  $nearest_dist = 0.0;
  $visited = 0;
  $i = 0;
  while ($i < count($tree)) {
  $node = $tree[$i];
  $dist = square_distance($query_point, $node['point']);
  $visited = $visited + 1;
  if ($visited == 1 || $dist < $nearest_dist) {
  $nearest_point = $node['point'];
  $nearest_dist = $dist;
}
  $i = $i + 1;
};
  return ['point' => $nearest_point, 'dist' => $nearest_dist, 'visited' => $visited];
}
function list_to_string($arr) {
  global $seed;
  $s = '[';
  $i = 0;
  while ($i < count($arr)) {
  $s = $s . _str($arr[$i]);
  if ($i < count($arr) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function main() {
  global $seed;
  $num_points = 5000;
  $cube_size = 10.0;
  $num_dimensions = 10;
  $pts = hypercube_points($num_points, $cube_size, $num_dimensions);
  $build = build_kdtree($pts, 0);
  $root = $build['index'];
  $tree = $build['nodes'];
  $query = [];
  $i = 0;
  while ($i < $num_dimensions) {
  $query = _append($query, random());
  $i = $i + 1;
};
  $res = nearest_neighbour_search($tree, $root, $query);
  echo rtrim('Query point: ' . list_to_string($query)), PHP_EOL;
  echo rtrim('Nearest point: ' . list_to_string($res['point'])), PHP_EOL;
  echo rtrim('Distance: ' . _str($res['dist'])), PHP_EOL;
  echo rtrim('Nodes visited: ' . _str($res['visited'])), PHP_EOL;
}
main();
