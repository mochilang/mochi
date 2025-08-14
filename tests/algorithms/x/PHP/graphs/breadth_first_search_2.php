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
  function mochi_join($xs) {
  global $G;
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  $i = $i + 1;
};
  return $s;
};
  function breadth_first_search($graph, $start) {
  global $G;
  $explored = [];
  $explored[$start] = true;
  $result = [$start];
  $queue = [$start];
  while (count($queue) > 0) {
  $v = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  $children = $graph[$v];
  $i = 0;
  while ($i < count($children)) {
  $w = $children[$i];
  if (!(array_key_exists($w, $explored))) {
  $explored[$w] = true;
  $result = _append($result, $w);
  $queue = _append($queue, $w);
}
  $i = $i + 1;
};
};
  return $result;
};
  function breadth_first_search_with_deque($graph, $start) {
  global $G;
  $visited = [];
  $visited[$start] = true;
  $result = [$start];
  $queue = [$start];
  $head = 0;
  while ($head < count($queue)) {
  $v = $queue[$head];
  $head = $head + 1;
  $children = $graph[$v];
  $i = 0;
  while ($i < count($children)) {
  $child = $children[$i];
  if (!(array_key_exists($child, $visited))) {
  $visited[$child] = true;
  $result = _append($result, $child);
  $queue = _append($queue, $child);
}
  $i = $i + 1;
};
};
  return $result;
};
  $G = ['A' => ['B', 'C'], 'B' => ['A', 'D', 'E'], 'C' => ['A', 'F'], 'D' => ['B'], 'E' => ['B', 'F'], 'F' => ['C', 'E']];
  echo rtrim(json_encode(mochi_join(breadth_first_search($G, 'A')), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join(breadth_first_search_with_deque($G, 'A')), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
