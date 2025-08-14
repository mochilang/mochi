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
  function mochi_contains($lst, $v) {
  global $G, $result;
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] == $v) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function depth_first_search($graph, $start) {
  global $G, $result;
  $explored = [];
  $stack = [];
  $stack = _append($stack, $start);
  $explored = _append($explored, $start);
  while (count($stack) > 0) {
  $idx = count($stack) - 1;
  $v = $stack[$idx];
  $stack = array_slice($stack, 0, $idx);
  $neighbors = $graph[$v];
  $i = count($neighbors) - 1;
  while ($i >= 0) {
  $adj = $neighbors[$i];
  if (!mochi_contains($explored, $adj)) {
  $explored = _append($explored, $adj);
  $stack = _append($stack, $adj);
}
  $i = $i - 1;
};
};
  return $explored;
};
  $G = ['A' => ['B', 'C', 'D'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F'], 'D' => ['B', 'D'], 'E' => ['B', 'F'], 'F' => ['C', 'E', 'G'], 'G' => ['F']];
  $result = depth_first_search($G, 'A');
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($result, 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
