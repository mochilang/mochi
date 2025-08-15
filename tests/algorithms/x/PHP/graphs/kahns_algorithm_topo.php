<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function topological_sort($graph) {
  $indegree = [];
  $i = 0;
  while ($i < count($graph)) {
  $indegree = _append($indegree, 0);
  $i = $i + 1;
};
  foreach (array_values($graph) as $edges) {
  $j = 0;
  while ($j < count($edges)) {
  $v = $edges[$j];
  $indegree[$v] = $indegree[$v] + 1;
  $j = $j + 1;
};
};
  $queue = [];
  $i = 0;
  while ($i < count($indegree)) {
  if ($indegree[$i] == 0) {
  $queue = _append($queue, $i);
}
  $i = $i + 1;
};
  $order = [];
  $head = 0;
  $processed = 0;
  while ($head < count($queue)) {
  $v = $queue[$head];
  $head = $head + 1;
  $processed = $processed + 1;
  $order = _append($order, $v);
  $neighbors = $graph[$v];
  $k = 0;
  while ($k < count($neighbors)) {
  $nb = $neighbors[$k];
  $indegree[$nb] = $indegree[$nb] - 1;
  if ($indegree[$nb] == 0) {
  $queue = _append($queue, $nb);
}
  $k = $k + 1;
};
};
  if ($processed != count($graph)) {
  return null;
}
  return $order;
}
function main() {
  $graph = [0 => [1, 2], 1 => [3], 2 => [3], 3 => [4, 5], 4 => [], 5 => []];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(topological_sort($graph), 1344)))))), PHP_EOL;
  $cyclic = [0 => [1], 1 => [2], 2 => [0]];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(topological_sort($cyclic), 1344)))))), PHP_EOL;
}
main();
