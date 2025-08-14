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
  function mochi_contains($xs, $x) {
  global $demo_graph;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function contains_key($m, $key) {
  global $demo_graph;
  foreach (array_keys($m) as $k) {
  if ($k == $key) {
  return true;
}
};
  return false;
};
  function bfs_shortest_path($graph, $start, $goal) {
  global $demo_graph;
  $explored = [];
  $queue = [[$start]];
  if ($start == $goal) {
  return [$start];
}
  while (count($queue) > 0) {
  $path = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  $node = $path[count($path) - 1];
  if (!mochi_contains($explored, $node)) {
  $neighbours = $graph[$node];
  $i = 0;
  while ($i < count($neighbours)) {
  $neighbour = $neighbours[$i];
  $new_path = $path;
  $new_path = _append($new_path, $neighbour);
  $queue = _append($queue, $new_path);
  if ($neighbour == $goal) {
  return $new_path;
}
  $i = $i + 1;
};
  $explored = _append($explored, $node);
}
};
  return [];
};
  function bfs_shortest_path_distance($graph, $start, $target) {
  global $demo_graph;
  if ((contains_key($graph, $start) == false) || (contains_key($graph, $target) == false)) {
  return -1;
}
  if ($start == $target) {
  return 0;
}
  $queue = [$start];
  $visited = [$start];
  $dist = [];
  $dist[$start] = 0;
  $dist[$target] = (-1);
  while (count($queue) > 0) {
  $node = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  if ($node == $target) {
  if ($dist[$target] == (-1) || $dist[$node] < $dist[$target]) {
  $dist[$target] = $dist[$node];
};
}
  $adj = $graph[$node];
  $i = 0;
  while ($i < count($adj)) {
  $next = $adj[$i];
  if (!mochi_contains($visited, $next)) {
  $visited = _append($visited, $next);
  $queue = _append($queue, $next);
  $dist[$next] = $dist[$node] + 1;
}
  $i = $i + 1;
};
};
  return $dist[$target];
};
  $demo_graph = ['A' => ['B', 'C', 'E'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F', 'G'], 'D' => ['B'], 'E' => ['A', 'B', 'D'], 'F' => ['C'], 'G' => ['C']];
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
