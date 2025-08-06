<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function dfs($u, $graph, &$visit, $stack) {
  if ($visit[$u]) {
  return $stack;
}
  $visit[$u] = true;
  foreach ($graph[$u] as $v) {
  $stack = dfs($v, $graph, $visit, $stack);
};
  $stack = _append($stack, $u);
  return $stack;
}
function dfs2($u, $reversed_graph, &$visit, $component) {
  if ($visit[$u]) {
  return $component;
}
  $visit[$u] = true;
  $component = _append($component, $u);
  foreach ($reversed_graph[$u] as $v) {
  $component = dfs2($v, $reversed_graph, $visit, $component);
};
  return $component;
}
function kosaraju($graph) {
  $n = count($graph);
  $reversed_graph = [];
  $i = 0;
  while ($i < $n) {
  $reversed_graph = _append($reversed_graph, []);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  foreach ($graph[$i] as $v) {
  $reversed_graph[$v] = _append($reversed_graph[$v], $i);
};
  $i = $i + 1;
};
  $visit = [];
  $i = 0;
  while ($i < $n) {
  $visit = _append($visit, false);
  $i = $i + 1;
};
  $stack = [];
  $i = 0;
  while ($i < $n) {
  if ($visit[$i] == false) {
  $stack = dfs($i, $graph, $visit, $stack);
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  $visit[$i] = false;
  $i = $i + 1;
};
  $scc = [];
  $idx = count($stack) - 1;
  while ($idx >= 0) {
  $node = $stack[$idx];
  if ($visit[$node] == false) {
  $component = [];
  $component = dfs2($node, $reversed_graph, $visit, $component);
  $scc = _append($scc, $component);
}
  $idx = $idx - 1;
};
  return $scc;
}
function main() {
  $graph = [[1], [2], [0, 3], [4], []];
  $comps = kosaraju($graph);
  $i = 0;
  while ($i < count($comps)) {
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($comps[$i], 1344)))))), PHP_EOL;
  $i = $i + 1;
};
}
main();
