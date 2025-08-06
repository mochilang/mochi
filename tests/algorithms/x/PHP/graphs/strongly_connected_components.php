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
function topology_sort($graph, $vert, &$visited) {
  $visited[$vert] = true;
  $order = [];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $order = array_merge($order, topology_sort($graph, $neighbour, $visited));
}
};
  $order = _append($order, $vert);
  return $order;
}
function find_component($graph, $vert, &$visited) {
  $visited[$vert] = true;
  $comp = [$vert];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $comp = array_merge($comp, find_component($graph, $neighbour, $visited));
}
};
  return $comp;
}
function strongly_connected_components($graph) {
  $n = count($graph);
  $visited = [];
  for ($_ = 0; $_ < $n; $_++) {
  $visited = _append($visited, false);
};
  $reversed = [];
  for ($_ = 0; $_ < $n; $_++) {
  $reversed = _append($reversed, []);
};
  for ($i = 0; $i < $n; $i++) {
  foreach ($graph[$i] as $neighbour) {
  $reversed[$neighbour] = _append($reversed[$neighbour], $i);
};
};
  $order = [];
  for ($i = 0; $i < $n; $i++) {
  if (!$visited[$i]) {
  $order = array_merge($order, topology_sort($graph, $i, $visited));
}
};
  $visited = [];
  for ($_ = 0; $_ < $n; $_++) {
  $visited = _append($visited, false);
};
  $components = [];
  $i = 0;
  while ($i < $n) {
  $v = $order[$n - $i - 1];
  if (!$visited[$v]) {
  $comp = find_component($reversed, $v, $visited);
  $components = _append($components, $comp);
}
  $i = $i + 1;
};
  return $components;
}
function main() {
  $test_graph_1 = [[2, 3], [0], [1], [4], []];
  $test_graph_2 = [[1, 2, 3], [2], [0], [4], [5], [3]];
  echo rtrim(_str(strongly_connected_components($test_graph_1))), PHP_EOL;
  echo rtrim(_str(strongly_connected_components($test_graph_2))), PHP_EOL;
}
main();
