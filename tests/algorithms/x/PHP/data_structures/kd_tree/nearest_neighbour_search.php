<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _len($x) {
    if ($x === null) { return 0; }
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
$__start_mem = memory_get_usage();
$__start = _now();
  function square_distance($a, $b) {
  global $nodes, $q, $queries, $res;
  $i = 0;
  $total = 0.0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $total = $total + $diff * $diff;
  $i = $i + 1;
};
  return $total;
};
  function search($nodes, $index, $query_point, $depth, $best) {
  global $q, $queries, $res;
  if ($index == (-1)) {
  return $best;
}
  $result = $best;
  $result['nodes_visited'] = $result['nodes_visited'] + 1;
  $node = $nodes[$index];
  $current_point = $node['point'];
  $current_dist = square_distance($query_point, $current_point);
  if (_len($result['point']) == 0 || $current_dist < $result['distance']) {
  $result['point'] = $current_point;
  $result['distance'] = $current_dist;
}
  $k = count($query_point);
  $axis = $depth % $k;
  $nearer = $node['left'];
  $further = $node['right'];
  if ($query_point[$axis] > $current_point[$axis]) {
  $nearer = $node['right'];
  $further = $node['left'];
}
  $result = search($nodes, $nearer, $query_point, $depth + 1, $result);
  $diff = $query_point[$axis] - $current_point[$axis];
  if ($diff * $diff < $result['distance']) {
  $result = search($nodes, $further, $query_point, $depth + 1, $result);
}
  return $result;
};
  function nearest_neighbour_search($nodes, $root, $query_point) {
  global $q, $queries, $res;
  $initial = ['distance' => 1000000000000000000000000000000.0, 'nodes_visited' => 0, 'point' => []];
  return search($nodes, $root, $query_point, 0, $initial);
};
  $nodes = [['left' => 1, 'point' => [9.0, 1.0], 'right' => 4], ['left' => 2, 'point' => [2.0, 7.0], 'right' => 3], ['left' => -1, 'point' => [3.0, 6.0], 'right' => -1], ['left' => -1, 'point' => [6.0, 12.0], 'right' => -1], ['left' => 5, 'point' => [17.0, 15.0], 'right' => 6], ['left' => -1, 'point' => [13.0, 15.0], 'right' => -1], ['left' => -1, 'point' => [10.0, 19.0], 'right' => -1]];
  $queries = [[9.0, 2.0], [12.0, 15.0], [1.0, 3.0]];
  $q = 0;
  while ($q < count($queries)) {
  $res = nearest_neighbour_search($nodes, 0, $queries[$q]);
  echo rtrim(_str($res['point']) . ' ' . _str($res['distance']) . ' ' . _str($res['nodes_visited']) . '
'), PHP_EOL;
  $q = $q + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
