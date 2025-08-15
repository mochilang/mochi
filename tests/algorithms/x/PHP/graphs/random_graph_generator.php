<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$seed = 1;
function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function random() {
  global $seed;
  return (1.0 * mochi_rand()) / 2147483648.0;
}
function complete_graph($vertices_number) {
  global $seed;
  $graph = [];
  $i = 0;
  while ($i < $vertices_number) {
  $neighbors = [];
  $j = 0;
  while ($j < $vertices_number) {
  if ($j != $i) {
  $neighbors = _append($neighbors, $j);
}
  $j = $j + 1;
};
  $graph[$i] = $neighbors;
  $i = $i + 1;
};
  return $graph;
}
function random_graph($vertices_number, $probability, $directed) {
  global $seed;
  $graph = [];
  $i = 0;
  while ($i < $vertices_number) {
  $graph[$i] = [];
  $i = $i + 1;
};
  if ($probability >= 1.0) {
  return complete_graph($vertices_number);
}
  if ($probability <= 0.0) {
  return $graph;
}
  $i = 0;
  while ($i < $vertices_number) {
  $j = $i + 1;
  while ($j < $vertices_number) {
  if (random() < $probability) {
  $graph[$i] = _append($graph[$i], $j);
  if (!$directed) {
  $graph[$j] = _append($graph[$j], $i);
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $graph;
}
function main() {
  global $seed;
  $seed = 1;
  $g1 = random_graph(4, 0.5, false);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($g1, 1344)))))), PHP_EOL;
  $seed = 1;
  $g2 = random_graph(4, 0.5, true);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($g2, 1344)))))), PHP_EOL;
}
main();
