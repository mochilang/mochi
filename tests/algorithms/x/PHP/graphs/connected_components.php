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
  $test_graph_1 = [0 => [1, 2], 1 => [0, 3], 2 => [0], 3 => [1], 4 => [5, 6], 5 => [4, 6], 6 => [4, 5]];
  $test_graph_2 = [0 => [1, 2, 3], 1 => [0, 3], 2 => [0], 3 => [0, 1], 4 => [], 5 => []];
  function dfs($graph, $vert, &$visited) {
  global $test_graph_1, $test_graph_2;
  $visited[$vert] = true;
  $connected_verts = [];
  foreach ($graph[$vert] as $neighbour) {
  if (!$visited[$neighbour]) {
  $connected_verts = array_merge($connected_verts, dfs($graph, $neighbour, $visited));
}
};
  return array_merge([$vert], $connected_verts);
};
  function connected_components($graph) {
  global $test_graph_1, $test_graph_2;
  $graph_size = count($graph);
  $visited = [];
  for ($_ = 0; $_ < $graph_size; $_++) {
  $visited = _append($visited, false);
};
  $components_list = [];
  for ($i = 0; $i < $graph_size; $i++) {
  if (!$visited[$i]) {
  $component = dfs($graph, $i, $visited);
  $components_list = _append($components_list, $component);
}
};
  return $components_list;
};
  echo rtrim(_str(connected_components($test_graph_1))), PHP_EOL;
  echo rtrim(_str(connected_components($test_graph_2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
