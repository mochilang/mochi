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
$test_graph_1 = [0 => [1, 2], 1 => [0, 3], 2 => [0], 3 => [1], 4 => [5, 6], 5 => [4, 6], 6 => [4, 5]];
$test_graph_2 = [0 => [1, 2, 3], 1 => [0, 3], 2 => [0], 3 => [0, 1], 4 => [], 5 => []];
function dfs($graph, $vert, &$visited) {
  global $test_graph_1, $test_graph_2;
  $visited[$vert] = true;
  $connected_verts = [];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $connected_verts = array_merge($connected_verts, dfs($graph, $neighbour, $visited));
}
};
  return array_merge([$vert], $connected_verts);
}
function connected_components($graph) {
  global $test_graph_1, $test_graph_2;
  $graph_size = count($graph);
  $visited = [];
  for ($_ = 0; $_ < $graph_size; $_++) {
  $visited = _append($visited, false);
};
  $components_list = [];
  for ($i = 0; $i < $graph_size; $i++) {
  if (!$visited[$i]) {
  $component = dfs($graph, $i, $visited);
  $components_list = _append($components_list, $component);
}
};
  return $components_list;
}
echo rtrim(_str(connected_components($test_graph_1))), PHP_EOL;
echo rtrim(_str(connected_components($test_graph_2))), PHP_EOL;
