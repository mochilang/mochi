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
  $es = $edges;
  $i = 0;
  while ($i < count($es)) {
  $j = 0;
  while ($j < count($es) - $i - 1) {
  if ($es[$j][2] > $es[$j + 1][2]) {
  $tmp = $es[$j];
  $es[$j] = $es[$j + 1];
  $es[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $es;
};
  function find($parent, $x) {
  $r = $x;
  while ($parent[$r] != $r) {
  $r = $parent[$r];
};
  return $r;
};
  function kruskal($n, $edges) {
  $parent = [];
  $i = 0;
  while ($i < $n) {
  $parent = _append($parent, $i);
  $i = $i + 1;
};
  $sorted = sort_edges($edges);
  $mst = [];
  $e = 0;
  while ($e < count($sorted)) {
  if (count($mst) == $n - 1) {
  break;
}
  $edge = $sorted[$e];
  $e = $e + 1;
  $u = $edge[0];
  $v = $edge[1];
  $w = $edge[2];
  $ru = find($parent, $u);
  $rv = find($parent, $v);
  if ($ru != $rv) {
  $parent[$ru] = $rv;
  $mst = _append($mst, [$u, $v, $w]);
}
};
  return $mst;
};
  function edges_equal($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  $e1 = $a[$i];
  $e2 = $b[$i];
  if ($e1[0] != $e2[0] || $e1[1] != $e2[1] || $e1[2] != $e2[2]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function main() {
  $num_nodes = 9;
  $edges = [[0, 1, 4], [0, 7, 8], [1, 2, 8], [7, 8, 7], [7, 6, 1], [2, 8, 2], [8, 6, 6], [2, 3, 7], [2, 5, 4], [6, 5, 2], [3, 5, 14], [3, 4, 9], [5, 4, 10], [1, 7, 11]];
  $expected = [[7, 6, 1], [2, 8, 2], [6, 5, 2], [0, 1, 4], [2, 5, 4], [2, 3, 7], [0, 7, 8], [3, 4, 9]];
  $result = kruskal($num_nodes, $edges);
  $sorted_result = sort_edges($result);
  $sorted_expected = sort_edges($expected);
  echo rtrim(_str($sorted_result)), PHP_EOL;
  if (edges_equal($sorted_expected, $sorted_result)) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
