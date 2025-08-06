<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function dfs_skip($graph, &$visited, $skip, $at) {
  $visited[$at] = true;
  $count = 1;
  foreach ($graph[$at] as $to) {
  if ($to == $skip) {
  continue;
}
  if ($visited[$to] == false) {
  $count = $count + dfs_skip($graph, $visited, $skip, $to);
}
};
  return $count;
}
function articulation_points($graph) {
  $n = count($graph);
  $result = [];
  $v = 0;
  while ($v < $n) {
  $visited = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $i = $i + 1;
};
  $start = 0;
  while ($start == $v && $start < $n) {
  $start = $start + 1;
};
  $reach = dfs_skip($graph, $visited, $v, $start);
  if ($reach < $n - 1) {
  $result = _append($result, $v);
  echo rtrim(json_encode($v, 1344)), PHP_EOL;
}
  $v = $v + 1;
};
  return $result;
}
function main() {
  $graph = [[1, 2], [0, 2], [0, 1, 3, 5], [2, 4], [3], [2, 6, 8], [5, 7], [6, 8], [5, 7]];
  articulation_points($graph);
}
main();
