<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function dfs($graph, $at, $parent, &$visited, &$ids, &$low, $id, $bridges) {
  $visited[$at] = true;
  $ids[$at] = $id;
  $low[$at] = $id;
  $current_id = $id + 1;
  $res_bridges = $bridges;
  foreach ($graph[$at] as $to) {
  if ($to == $parent) {
  continue;
} else {
  if (!$visited[$to]) {
  $result = dfs($graph, $to, $at, $visited, $ids, $low, $current_id, $res_bridges);
  $current_id = $result['id'];
  $res_bridges = $result['bridges'];
  if ($low[$at] > $low[$to]) {
  $low[$at] = $low[$to];
};
  if ($ids[$at] < $low[$to]) {
  $edge = ($at < $to ? [$at, $to] : [$to, $at]);
  $res_bridges = _append($res_bridges, $edge);
};
} else {
  if ($low[$at] > $ids[$to]) {
  $low[$at] = $ids[$to];
};
};
}
};
  return ['id' => $current_id, 'bridges' => $res_bridges];
}
function compute_bridges($graph) {
  $n = count($graph);
  $visited = [];
  $ids = [];
  $low = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $ids = _append($ids, 0);
  $low = _append($low, 0);
  $i = $i + 1;
};
  $bridges = [];
  $id = 0;
  $i = 0;
  while ($i < $n) {
  if (!$visited[$i]) {
  $result = dfs($graph, $i, -1, $visited, $ids, $low, $id, $bridges);
  $id = $result['id'];
  $bridges = $result['bridges'];
}
  $i = $i + 1;
};
  return $bridges;
}
function get_demo_graph($index) {
  if ($index == 0) {
  return [0 => [1, 2], 1 => [0, 2], 2 => [0, 1, 3, 5], 3 => [2, 4], 4 => [3], 5 => [2, 6, 8], 6 => [5, 7], 7 => [6, 8], 8 => [5, 7]];
}
  if ($index == 1) {
  return [0 => [6], 1 => [9], 2 => [4, 5], 3 => [4], 4 => [2, 3], 5 => [2], 6 => [0, 7], 7 => [6], 8 => [], 9 => [1]];
}
  if ($index == 2) {
  return [0 => [4], 1 => [6], 2 => [], 3 => [5, 6, 7], 4 => [0, 6], 5 => [3, 8, 9], 6 => [1, 3, 4, 7], 7 => [3, 6, 8, 9], 8 => [5, 7], 9 => [5, 7]];
}
  return [0 => [1, 3], 1 => [0, 2, 4], 2 => [1, 3, 4], 3 => [0, 2, 4], 4 => [1, 2, 3]];
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(0)), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(1)), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(2)), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(3)), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges([]), 1344)))))), PHP_EOL;
