<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function contains($lst, $v) {
  global $G, $result;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $v) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function depth_first_search($graph, $start) {
  global $G, $result;
  $explored = [];
  $stack = [];
  $stack = _append($stack, $start);
  $explored = _append($explored, $start);
  while (count($stack) > 0) {
  $idx = count($stack) - 1;
  $v = $stack[$idx];
  $stack = array_slice($stack, 0, $idx - 0);
  $neighbors = $graph[$v];
  $i = count($neighbors) - 1;
  while ($i >= 0) {
  $adj = $neighbors[$i];
  if (!contains($explored, $adj)) {
  $explored = _append($explored, $adj);
  $stack = _append($stack, $adj);
}
  $i = $i - 1;
};
};
  return $explored;
}
$G = ['A' => ['B', 'C', 'D'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F'], 'D' => ['B', 'D'], 'E' => ['B', 'F'], 'F' => ['C', 'E', 'G'], 'G' => ['F']];
$result = depth_first_search($G, 'A');
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($result, 1344)))))), PHP_EOL;
