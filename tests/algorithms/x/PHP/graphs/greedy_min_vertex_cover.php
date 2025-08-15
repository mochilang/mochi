<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function remove_value($lst, $val) {
  global $graph;
  $res = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] != $val) {
  $res = _append($res, $lst[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function greedy_min_vertex_cover($graph) {
  $g = $graph;
  $cover = [];
  while (true) {
  $max_v = 0;
  $max_deg = 0;
  foreach (array_keys($g) as $v) {
  $key = intval($v);
  $deg = count($g[$key]);
  if ($deg > $max_deg) {
  $max_deg = $deg;
  $max_v = $key;
}
};
  if ($max_deg == 0) {
  break;
}
  $cover = _append($cover, $max_v);
  $neighbors = $g[$max_v];
  $i = 0;
  while ($i < count($neighbors)) {
  $n = $neighbors[$i];
  $g[$n] = remove_value($g[$n], $max_v);
  $i = $i + 1;
};
  $g[$max_v] = [];
};
  return $cover;
}
$graph = [0 => [1, 3], 1 => [0, 3], 2 => [0, 3, 4], 3 => [0, 1, 2], 4 => [2, 3]];
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(greedy_min_vertex_cover($graph), 1344)))))), PHP_EOL;
