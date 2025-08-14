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
};
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
};
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
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(0)), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(1)), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(2)), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges(get_demo_graph(3)), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(compute_bridges([]), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
