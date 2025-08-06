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
$INF = 1000000000.0;
function list_to_string($arr) {
  global $INF, $edges, $distances;
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
function check_negative_cycle($graph, $distance, $edge_count) {
  global $INF, $edges, $distances;
  $j = 0;
  while ($j < $edge_count) {
  $e = $graph[$j];
  $u = $e['src'];
  $v = $e['dst'];
  $w = floatval($e['weight']);
  if ($distance[$u] < $INF && $distance[$u] + $w < $distance[$v]) {
  return true;
}
  $j = $j + 1;
};
  return false;
}
function bellman_ford($graph, $vertex_count, $edge_count, $src) {
  global $INF, $edges, $distances;
  $distance = [];
  $i = 0;
  while ($i < $vertex_count) {
  $distance = _append($distance, $INF);
  $i = $i + 1;
};
  $distance[$src] = 0.0;
  $k = 0;
  while ($k < $vertex_count - 1) {
  $j = 0;
  while ($j < $edge_count) {
  $e = $graph[$j];
  $u = $e['src'];
  $v = $e['dst'];
  $w = floatval($e['weight']);
  if ($distance[$u] < $INF && $distance[$u] + $w < $distance[$v]) {
  $distance[$v] = $distance[$u] + $w;
}
  $j = $j + 1;
};
  $k = $k + 1;
};
  if (check_negative_cycle($graph, $distance, $edge_count)) {
  $panic('Negative cycle found');
}
  return $distance;
}
$edges = [['src' => 2, 'dst' => 1, 'weight' => -10], ['src' => 3, 'dst' => 2, 'weight' => 3], ['src' => 0, 'dst' => 3, 'weight' => 5], ['src' => 0, 'dst' => 1, 'weight' => 4]];
$distances = bellman_ford($edges, 4, count($edges), 0);
echo rtrim(list_to_string($distances)), PHP_EOL;
