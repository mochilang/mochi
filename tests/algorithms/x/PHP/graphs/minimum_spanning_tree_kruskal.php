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
  function sort_edges($edges) {
  global $edges1, $edges2, $edges3;
  $es = $edges;
  $i = 0;
  while ($i < count($es)) {
  $j = 0;
  while ($j < count($es) - $i - 1) {
  if ($es[$j][2] > $es[$j + 1][2]) {
  $temp = $es[$j];
  $es[$j] = $es[$j + 1];
  $es[$j + 1] = $temp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $es;
};
  function &find_parent(&$parent, $i) {
  global $edges1, $edges2, $edges3;
  if ($parent[$i] != $i) {
  $parent[$i] = find_parent($parent, $parent[$i]);
}
  return $parent[$i];
};
  function kruskal($num_nodes, $edges) {
  global $edges1, $edges2, $edges3;
  $es = sort_edges($edges);
  $parent = [];
  $i = 0;
  while ($i < $num_nodes) {
  $parent = _append($parent, $i);
  $i = $i + 1;
};
  $mst = [];
  $idx = 0;
  while ($idx < count($es)) {
  $e = $es[$idx];
  $pa =& find_parent($parent, $e[0]);
  $pb =& find_parent($parent, $e[1]);
  if ($pa != $pb) {
  $mst = _append($mst, $e);
  $parent[$pa] = $pb;
}
  $idx = $idx + 1;
};
  return $mst;
};
  function edges_to_string($es) {
  global $edges1, $edges2, $edges3;
  $s = '[';
  $i = 0;
  while ($i < count($es)) {
  $e = $es[$i];
  $s = $s . '(' . _str($e[0]) . ', ' . _str($e[1]) . ', ' . _str($e[2]) . ')';
  if ($i < count($es) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  $edges1 = [[0, 1, 3], [1, 2, 5], [2, 3, 1]];
  echo rtrim(edges_to_string(kruskal(4, $edges1))), PHP_EOL;
  $edges2 = [[0, 1, 3], [1, 2, 5], [2, 3, 1], [0, 2, 1], [0, 3, 2]];
  echo rtrim(edges_to_string(kruskal(4, $edges2))), PHP_EOL;
  $edges3 = [[0, 1, 3], [1, 2, 5], [2, 3, 1], [0, 2, 1], [0, 3, 2], [2, 1, 1]];
  echo rtrim(edges_to_string(kruskal(4, $edges3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
