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
  function depth_first_search($graph, $vertex, &$visited, &$rec_stk) {
  global $g1, $g2;
  $visited[$vertex] = true;
  $rec_stk[$vertex] = true;
  foreach ($graph[$vertex] as $node) {
  if (!$visited[$node]) {
  if (depth_first_search($graph, $node, $visited, $rec_stk)) {
  return true;
};
} else {
  if ($rec_stk[$node]) {
  return true;
};
}
};
  $rec_stk[$vertex] = false;
  return false;
};
  function check_cycle($graph) {
  global $g1, $g2;
  $n = count($graph);
  $visited = [];
  $rec_stk = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $rec_stk = _append($rec_stk, false);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $n) {
  if (!$visited[$i]) {
  if (depth_first_search($graph, $i, $visited, $rec_stk)) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
};
  function print_bool($b) {
  global $g1, $g2;
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  $g1 = [[], [0, 3], [0, 4], [5], [5], []];
  print_bool(check_cycle($g1));
  $g2 = [[1, 2], [2], [0, 3], [3]];
  print_bool(check_cycle($g2));
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
