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
function minimum_distance($distances, $visited) {
  global $graph;
  $minimum = 10000000;
  $min_index = 0;
  $vertex = 0;
  while ($vertex < count($distances)) {
  if ($distances[$vertex] < $minimum && $visited[$vertex] == false) {
  $minimum = $distances[$vertex];
  $min_index = $vertex;
}
  $vertex = $vertex + 1;
};
  return $min_index;
}
function dijkstra($graph, $source) {
  $vertices = count($graph);
  $distances = null;
  $i = 0;
  while ($i < $vertices) {
  $distances = _append($distances, 10000000);
  $i = $i + 1;
};
  $distances[$source] = 0;
  $visited = null;
  $i = 0;
  while ($i < $vertices) {
  $visited = _append($visited, false);
  $i = $i + 1;
};
  $count = 0;
  while ($count < $vertices) {
  $u = minimum_distance($distances, $visited);
  $visited[$u] = true;
  $v = 0;
  while ($v < $vertices) {
  if ($graph[$u][$v] > 0 && $visited[$v] == false && $distances[$v] > $distances[$u] + $graph[$u][$v]) {
  $distances[$v] = $distances[$u] + $graph[$u][$v];
}
  $v = $v + 1;
};
  $count = $count + 1;
};
  return $distances;
}
function print_solution($distances) {
  global $graph;
  echo rtrim('Vertex 	 Distance from Source'), PHP_EOL;
  $v = 0;
  while ($v < count($distances)) {
  echo rtrim(_str($v) . '		' . _str($distances[$v])), PHP_EOL;
  $v = $v + 1;
};
}
$graph = [[0, 4, 0, 0, 0, 0, 0, 8, 0], [4, 0, 8, 0, 0, 0, 0, 11, 0], [0, 8, 0, 7, 0, 4, 0, 0, 2], [0, 0, 7, 0, 9, 14, 0, 0, 0], [0, 0, 0, 9, 0, 10, 0, 0, 0], [0, 0, 4, 14, 10, 0, 2, 0, 0], [0, 0, 0, 0, 0, 2, 0, 1, 6], [8, 11, 0, 0, 0, 0, 1, 0, 7], [0, 0, 2, 0, 0, 0, 6, 7, 0]];
$distances = dijkstra($graph, 0);
print_solution($distances);
