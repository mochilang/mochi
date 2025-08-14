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
  function newGraph($g, $s) {
  global $graph;
  return ['graph' => $g, 'parent' => [], 'source' => $s];
};
  function breath_first_search($g) {
  global $graph;
  $parent = $g['parent'];
  $parent[$g['source']] = $g['source'];
  $queue = [$g['source']];
  $idx = 0;
  while ($idx < count($queue)) {
  $vertex = $queue[$idx];
  foreach ($g['graph'][$vertex] as $adj) {
  if (!(array_key_exists($adj, $parent))) {
  $parent[$adj] = $vertex;
  $queue = _append($queue, $adj);
}
};
  $idx = $idx + 1;
};
  $g['parent'] = $parent;
  return $g;
};
  function shortest_path($g, $target) {
  global $graph;
  if ($target == $g['source']) {
  return $g['source'];
}
  if (!(isset($g['parent'][$target]))) {
  return 'No path from vertex: ' . $g['source'] . ' to vertex: ' . $target;
}
  $p = $g['parent'][$target];
  return shortest_path($g, $p) . '->' . $target;
};
  $graph = ['A' => ['B', 'C', 'E'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F', 'G'], 'D' => ['B'], 'E' => ['A', 'B', 'D'], 'F' => ['C'], 'G' => ['C']];
  $g = newGraph($graph, 'G');
  $g = breath_first_search($g);
  echo rtrim(shortest_path($g, 'D')), PHP_EOL;
  echo rtrim(shortest_path($g, 'G')), PHP_EOL;
  echo rtrim(shortest_path($g, 'Foo')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
