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
  function mochi_contains($xs, $v) {
  global $cover, $graph;
  foreach ($xs as $x) {
  if ($x == $v) {
  return true;
}
};
  return false;
};
  function get_edges($graph) {
  global $cover;
  $n = count($graph);
  $edges = [];
  for ($i = 0; $i < $n; $i++) {
  foreach ($graph[$i] as $j) {
  $edges = _append($edges, [$i, $j]);
};
};
  return $edges;
};
  function matching_min_vertex_cover($graph) {
  global $cover;
  $chosen = [];
  $edges = get_edges($graph);
  while (count($edges) > 0) {
  $idx = count($edges) - 1;
  $e = $edges[$idx];
  $edges = array_slice($edges, 0, $idx);
  $u = $e[0];
  $v = $e[1];
  if (!mochi_contains($chosen, $u)) {
  $chosen = _append($chosen, $u);
}
  if (!mochi_contains($chosen, $v)) {
  $chosen = _append($chosen, $v);
}
  $filtered = [];
  foreach ($edges as $edge) {
  $a = $edge[0];
  $b = $edge[1];
  if ($a != $u && $b != $u && $a != $v && $b != $v) {
  $filtered = _append($filtered, $edge);
}
};
  $edges = $filtered;
};
  return $chosen;
};
  $graph = [0 => [1, 3], 1 => [0, 3], 2 => [0, 3, 4], 3 => [0, 1, 2], 4 => [2, 3]];
  $cover = matching_min_vertex_cover($graph);
  echo rtrim(_str($cover)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
