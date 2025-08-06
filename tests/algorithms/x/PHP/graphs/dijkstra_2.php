<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$INF = 1000000000.0;
function print_dist($dist) {
  global $INF;
  echo rtrim('Vertex Distance'), PHP_EOL;
  $i = 0;
  while ($i < count($dist)) {
  if ($dist[$i] >= $INF) {
  echo rtrim(json_encode($i, 1344)) . " " . rtrim('	INF'), PHP_EOL;
} else {
  echo rtrim(json_encode($i, 1344)) . " " . rtrim('	') . " " . rtrim(json_encode(intval($dist[$i]), 1344)), PHP_EOL;
}
  $i = $i + 1;
};
}
function min_dist($mdist, $vset) {
  global $INF;
  $min_val = $INF;
  $min_ind = -1;
  $i = 0;
  while ($i < count($mdist)) {
  if (!($vset[$i]) && $mdist[$i] < $min_val) {
  $min_val = $mdist[$i];
  $min_ind = $i;
}
  $i = $i + 1;
};
  return $min_ind;
}
function dijkstra($graph, $src) {
  global $INF;
  $v = count($graph);
  $mdist = [];
  $vset = [];
  $i = 0;
  while ($i < $v) {
  $mdist = _append($mdist, $INF);
  $vset = _append($vset, false);
  $i = $i + 1;
};
  $mdist[$src] = 0.0;
  $count = 0;
  while ($count < $v - 1) {
  $u = min_dist($mdist, $vset);
  $vset[$u] = true;
  $i = 0;
  while ($i < $v) {
  $alt = $mdist[$u] + $graph[$u][$i];
  if (!($vset[$i]) && $graph[$u][$i] < $INF && $alt < $mdist[$i]) {
  $mdist[$i] = $alt;
}
  $i = $i + 1;
};
  $count = $count + 1;
};
  return $mdist;
}
function main() {
  global $INF;
  $graph = [[0.0, 10.0, $INF, $INF, 5.0], [$INF, 0.0, 1.0, $INF, 2.0], [$INF, $INF, 0.0, 4.0, $INF], [$INF, $INF, 6.0, 0.0, $INF], [$INF, 3.0, 9.0, 2.0, 0.0]];
  $dist = dijkstra($graph, 0);
  print_dist($dist);
}
main();
