<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
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
};
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
};
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
};
  function main() {
  $graph = [[1], [2], [0, 3], [4], []];
  $comps = kosaraju($graph);
  $i = 0;
  while ($i < count($comps)) {
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($comps[$i], 1344)))))), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
