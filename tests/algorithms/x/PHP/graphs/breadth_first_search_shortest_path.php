<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function newGraph($g, $s) {
  global $graph;
  return ['graph' => $g, 'parent' => [], 'source' => $s];
}
function breath_first_search(&$g) {
  global $graph;
  $parent = $g['parent'];
  $parent[$g['source']] = $g['source'];
  $queue = [$g['source']];
  $idx = 0;
  while ($idx < count($queue)) {
  $vertex = $queue[$idx];
  foreach ($g['graph'][$vertex] as $adj) {
  if (!(array_key_exists($adj, $parent))) {
  $parent[$adj] = $vertex;
  $queue = _append($queue, $adj);
}
};
  $idx = $idx + 1;
};
  $g['parent'] = $parent;
  return $g;
}
function shortest_path($g, $target) {
  global $graph;
  if ($target == $g['source']) {
  return $g['source'];
}
  if (!(isset($g['parent'][$target]))) {
  return 'No path from vertex: ' . $g['source'] . ' to vertex: ' . $target;
}
  $p = $g['parent'][$target];
  return shortest_path($g, $p) . '->' . $target;
}
$graph = ['A' => ['B', 'C', 'E'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F', 'G'], 'D' => ['B'], 'E' => ['A', 'B', 'D'], 'F' => ['C'], 'G' => ['C']];
$g = newGraph($graph, 'G');
$g = breath_first_search($g);
echo rtrim(shortest_path($g, 'D')), PHP_EOL;
echo rtrim(shortest_path($g, 'G')), PHP_EOL;
echo rtrim(shortest_path($g, 'Foo')), PHP_EOL;
