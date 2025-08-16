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
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function topology_sort($graph, $vert, &$visited) {
  $visited[$vert] = true;
  $order = [];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $order = array_merge($order, topology_sort($graph, $neighbour, $visited));
}
};
  $order = _append($order, $vert);
  return $order;
};
  function find_component($graph, $vert, &$visited) {
  $visited[$vert] = true;
  $comp = [$vert];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $comp = array_merge($comp, find_component($graph, $neighbour, $visited));
}
};
  return $comp;
};
  function strongly_connected_components($graph) {
  $n = count($graph);
  $visited = [];
  for ($_ = 0; $_ < $n; $_++) {
  $visited = _append($visited, false);
};
  $reversed = [];
  for ($_ = 0; $_ < $n; $_++) {
  $reversed = _append($reversed, []);
};
  for ($i = 0; $i < $n; $i++) {
  foreach ($graph[$i] as $neighbour) {
  $reversed[$neighbour] = _append($reversed[$neighbour], $i);
};
};
  $order = [];
  for ($i = 0; $i < $n; $i++) {
  if (!$visited[$i]) {
  $order = array_merge($order, topology_sort($graph, $i, $visited));
}
};
  $visited = [];
  for ($_ = 0; $_ < $n; $_++) {
  $visited = _append($visited, false);
};
  $components = [];
  $i = 0;
  while ($i < $n) {
  $v = $order[$n - $i - 1];
  if (!$visited[$v]) {
  $comp = find_component($reversed, $v, $visited);
  $components = _append($components, $comp);
}
  $i = $i + 1;
};
  return $components;
};
  function main() {
  $test_graph_1 = [[2, 3], [0], [1], [4], []];
  $test_graph_2 = [[1, 2, 3], [2], [0], [4], [5], [3]];
  echo rtrim(_str(strongly_connected_components($test_graph_1))), PHP_EOL;
  echo rtrim(_str(strongly_connected_components($test_graph_2))), PHP_EOL;
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
