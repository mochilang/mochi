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
  function longest_distance($graph) {
  $n = count($graph);
  $indegree = [];
  $i = 0;
  while ($i < $n) {
  $indegree = _append($indegree, 0);
  $i = $i + 1;
};
  $long_dist = [];
  $j = 0;
  while ($j < $n) {
  $long_dist = _append($long_dist, 1);
  $j = $j + 1;
};
  $u = 0;
  while ($u < $n) {
  foreach ($graph[$u] as $v) {
  $indegree[$v] = $indegree[$v] + 1;
};
  $u = $u + 1;
};
  $queue = [];
  $head = 0;
  $k = 0;
  while ($k < $n) {
  if ($indegree[$k] == 0) {
  $queue = _append($queue, $k);
}
  $k = $k + 1;
};
  while ($head < count($queue)) {
  $vertex = $queue[$head];
  $head = $head + 1;
  foreach ($graph[$vertex] as $x) {
  $indegree[$x] = $indegree[$x] - 1;
  $new_dist = $long_dist[$vertex] + 1;
  if ($new_dist > $long_dist[$x]) {
  $long_dist[$x] = $new_dist;
}
  if ($indegree[$x] == 0) {
  $queue = _append($queue, $x);
}
};
};
  $max_len = $long_dist[0];
  $m = 1;
  while ($m < $n) {
  if ($long_dist[$m] > $max_len) {
  $max_len = $long_dist[$m];
}
  $m = $m + 1;
};
  return $max_len;
};
  $graph = [[2, 3, 4], [2, 7], [5], [5, 7], [7], [6], [7], []];
  echo rtrim(json_encode(longest_distance($graph), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
