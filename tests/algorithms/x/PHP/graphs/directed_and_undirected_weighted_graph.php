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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function list_contains_int($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function edge_exists($edges, $w, $v) {
  $i = 0;
  while ($i < count($edges)) {
  if ($edges[$i][0] == $w && $edges[$i][1] == $v) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function first_key($m) {
  foreach (array_keys($m) as $k) {
  return $k;
};
  return 0;
}
function rand_range($low, $high) {
  return (fmod(_now(), ($high - $low))) + $low;
}
function dg_make_graph() {
  return ['graph' => []];
}
function dg_add_pair(&$g, $u, $v, $w) {
  if (isset($g['graph'][$u])) {
  $edges = $g['graph'][$u];
  if (!edge_exists($edges, $w, $v)) {
  $edges = _append($edges, [$w, $v]);
  $m = $g['graph'];
  $m[$u] = $edges;
  $g['graph'] = $m;
};
} else {
  $m0 = $g['graph'];
  $m0[$u] = [[$w, $v]];
  $g['graph'] = $m0;
}
  if (!(isset($g['graph'][$v]))) {
  $m1 = $g['graph'];
  $m1[$v] = [];
  $g['graph'] = $m1;
}
}
function dg_remove_pair(&$g, $u, $v) {
  if (isset($g['graph'][$u])) {
  $edges = $g['graph'][$u];
  $new_edges = [];
  $i = 0;
  while ($i < count($edges)) {
  if ($edges[$i][1] != $v) {
  $new_edges = _append($new_edges, $edges[$i]);
}
  $i = $i + 1;
};
  $m = $g['graph'];
  $m[$u] = $new_edges;
  $g['graph'] = $m;
}
}
function dg_all_nodes($g) {
  $res = [];
  foreach (array_keys($g['graph']) as $k) {
  $res = _append($res, $k);
};
  return $res;
}
function dg_dfs_util($g, $node, &$visited, $order, $d) {
  $visited[$node] = true;
  $order = _append($order, $node);
  if ($d != (-1) && $node == $d) {
  return $order;
}
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $order = dg_dfs_util($g, $neigh, $visited, $order, $d);
  if ($d != (-1) && $order[count($order) - 1] == $d) {
  return $order;
};
}
  $i = $i + 1;
};
  return $order;
}
function dg_dfs($g, $s, $d) {
  if ($s == $d) {
  return [];
}
  $start = ($s == (-2) ? first_key($g['graph']) : $s);
  $visited = [];
  $order = [];
  $order = dg_dfs_util($g, $start, $visited, $order, $d);
  return $order;
}
function dg_bfs($g, $s) {
  $queue = [];
  $visited = [];
  $order = [];
  $start = ($s == (-2) ? first_key($g['graph']) : $s);
  $queue = _append($queue, $start);
  $visited[$start] = true;
  while (count($queue) > 0) {
  $node = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  $order = _append($order, $node);
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $queue = _append($queue, $neigh);
  $visited[$neigh] = true;
}
  $i = $i + 1;
};
};
  return $order;
}
function dg_in_degree($g, $u) {
  $count = 0;
  foreach (array_keys($g['graph']) as $k) {
  $edges = $g['graph'][$k];
  $i = 0;
  while ($i < count($edges)) {
  if ($edges[$i][1] == $u) {
  $count = $count + 1;
}
  $i = $i + 1;
};
};
  return $count;
}
function dg_out_degree($g, $u) {
  if (isset($g['graph'][$u])) {
  return _len($g['graph'][$u]);
}
  return 0;
}
function dg_topo_util($g, $node, &$visited, $stack) {
  $visited[$node] = true;
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $stack = dg_topo_util($g, $neigh, $visited, $stack);
}
  $i = $i + 1;
};
  $stack = _append($stack, $node);
  return $stack;
}
function dg_topological_sort($g) {
  $visited = [];
  $stack = [];
  foreach (array_keys($g['graph']) as $k) {
  if (!(array_key_exists($k, $visited))) {
  $stack = dg_topo_util($g, $k, $visited, $stack);
}
};
  $res = [];
  $i = count($stack) - 1;
  while ($i >= 0) {
  $res = _append($res, $stack[$i]);
  $i = $i - 1;
};
  return $res;
}
function dg_cycle_util($g, $node, &$visited, &$rec, $res) {
  $visited[$node] = true;
  $rec[$node] = true;
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $res = dg_cycle_util($g, $neigh, $visited, $rec, $res);
} else {
  if ($rec[$neigh]) {
  if (!list_contains_int($res, $neigh)) {
  $res = _append($res, $neigh);
};
  if (!list_contains_int($res, $node)) {
  $res = _append($res, $node);
};
};
}
  $i = $i + 1;
};
  $rec[$node] = false;
  return $res;
}
function dg_cycle_nodes($g) {
  $visited = [];
  $rec = [];
  $res = [];
  foreach (array_keys($g['graph']) as $k) {
  if (!(array_key_exists($k, $visited))) {
  $res = dg_cycle_util($g, $k, $visited, $rec, $res);
}
};
  return $res;
}
function dg_has_cycle_util($g, $node, &$visited, &$rec) {
  $visited[$node] = true;
  $rec[$node] = true;
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  if (dg_has_cycle_util($g, $neigh, $visited, $rec)) {
  return true;
};
} else {
  if ($rec[$neigh]) {
  return true;
};
}
  $i = $i + 1;
};
  $rec[$node] = false;
  return false;
}
function dg_has_cycle($g) {
  $visited = [];
  $rec = [];
  foreach (array_keys($g['graph']) as $k) {
  if (!(array_key_exists($k, $visited))) {
  if (dg_has_cycle_util($g, $k, $visited, $rec)) {
  return true;
};
}
};
  return false;
}
function dg_fill_graph_randomly(&$g, $c) {
  $count = $c;
  if ($count == (-1)) {
  $count = rand_range(10, 10010);
}
  $i = 0;
  while ($i < $count) {
  $edge_count = rand_range(1, 103);
  $j = 0;
  while ($j < $edge_count) {
  $n = rand_range(0, $count);
  if ($n != $i) {
  dg_add_pair($g, $i, $n, 1);
}
  $j = $j + 1;
};
  $i = $i + 1;
};
}
function dg_dfs_time($g, $s, $e) {
  $begin = _now();
  dg_dfs($g, $s, $e);
  $end = _now();
  return $end - $begin;
}
function dg_bfs_time($g, $s) {
  $begin = _now();
  dg_bfs($g, $s);
  $end = _now();
  return $end - $begin;
}
function g_make_graph() {
  return ['graph' => []];
}
function g_add_pair(&$g, $u, $v, $w) {
  if (isset($g['graph'][$u])) {
  $edges = $g['graph'][$u];
  if (!edge_exists($edges, $w, $v)) {
  $edges = _append($edges, [$w, $v]);
  $m = $g['graph'];
  $m[$u] = $edges;
  $g['graph'] = $m;
};
} else {
  $m0 = $g['graph'];
  $m0[$u] = [[$w, $v]];
  $g['graph'] = $m0;
}
  if (isset($g['graph'][$v])) {
  $edges2 = $g['graph'][$v];
  if (!edge_exists($edges2, $w, $u)) {
  $edges2 = _append($edges2, [$w, $u]);
  $m2 = $g['graph'];
  $m2[$v] = $edges2;
  $g['graph'] = $m2;
};
} else {
  $m3 = $g['graph'];
  $m3[$v] = [[$w, $u]];
  $g['graph'] = $m3;
}
}
function g_remove_pair(&$g, $u, $v) {
  if (isset($g['graph'][$u])) {
  $edges = $g['graph'][$u];
  $new_edges = [];
  $i = 0;
  while ($i < count($edges)) {
  if ($edges[$i][1] != $v) {
  $new_edges = _append($new_edges, $edges[$i]);
}
  $i = $i + 1;
};
  $m = $g['graph'];
  $m[$u] = $new_edges;
  $g['graph'] = $m;
}
  if (isset($g['graph'][$v])) {
  $edges2 = $g['graph'][$v];
  $new_edges2 = [];
  $j = 0;
  while ($j < count($edges2)) {
  if ($edges2[$j][1] != $u) {
  $new_edges2 = _append($new_edges2, $edges2[$j]);
}
  $j = $j + 1;
};
  $m2 = $g['graph'];
  $m2[$v] = $new_edges2;
  $g['graph'] = $m2;
}
}
function g_all_nodes($g) {
  $res = [];
  foreach (array_keys($g['graph']) as $k) {
  $res = _append($res, $k);
};
  return $res;
}
function g_dfs_util($g, $node, &$visited, $order, $d) {
  $visited[$node] = true;
  $order = _append($order, $node);
  if ($d != (-1) && $node == $d) {
  return $order;
}
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $order = g_dfs_util($g, $neigh, $visited, $order, $d);
  if ($d != (-1) && $order[count($order) - 1] == $d) {
  return $order;
};
}
  $i = $i + 1;
};
  return $order;
}
function g_dfs($g, $s, $d) {
  if ($s == $d) {
  return [];
}
  $start = ($s == (-2) ? first_key($g['graph']) : $s);
  $visited = [];
  $order = [];
  $order = g_dfs_util($g, $start, $visited, $order, $d);
  return $order;
}
function g_bfs($g, $s) {
  $queue = [];
  $visited = [];
  $order = [];
  $start = ($s == (-2) ? first_key($g['graph']) : $s);
  $queue = _append($queue, $start);
  $visited[$start] = true;
  while (count($queue) > 0) {
  $node = $queue[0];
  $queue = array_slice($queue, 1, count($queue) - 1);
  $order = _append($order, $node);
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $queue = _append($queue, $neigh);
  $visited[$neigh] = true;
}
  $i = $i + 1;
};
};
  return $order;
}
function g_degree($g, $u) {
  if (isset($g['graph'][$u])) {
  return _len($g['graph'][$u]);
}
  return 0;
}
function g_cycle_util($g, $node, &$visited, $parent, $res) {
  $visited[$node] = true;
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  $res = g_cycle_util($g, $neigh, $visited, $node, $res);
} else {
  if ($neigh != $parent) {
  if (!list_contains_int($res, $neigh)) {
  $res = _append($res, $neigh);
};
  if (!list_contains_int($res, $node)) {
  $res = _append($res, $node);
};
};
}
  $i = $i + 1;
};
  return $res;
}
function g_cycle_nodes($g) {
  $visited = [];
  $res = [];
  foreach (array_keys($g['graph']) as $k) {
  if (!(array_key_exists($k, $visited))) {
  $res = g_cycle_util($g, $k, $visited, -1, $res);
}
};
  return $res;
}
function g_has_cycle_util($g, $node, &$visited, $parent) {
  $visited[$node] = true;
  $edges = $g['graph'][$node];
  $i = 0;
  while ($i < count($edges)) {
  $neigh = $edges[$i][1];
  if (!(array_key_exists($neigh, $visited))) {
  if (g_has_cycle_util($g, $neigh, $visited, $node)) {
  return true;
};
} else {
  if ($neigh != $parent) {
  return true;
};
}
  $i = $i + 1;
};
  return false;
}
function g_has_cycle($g) {
  $visited = [];
  foreach (array_keys($g['graph']) as $k) {
  if (!(array_key_exists($k, $visited))) {
  if (g_has_cycle_util($g, $k, $visited, -1)) {
  return true;
};
}
};
  return false;
}
function g_fill_graph_randomly(&$g, $c) {
  $count = $c;
  if ($count == (-1)) {
  $count = rand_range(10, 10010);
}
  $i = 0;
  while ($i < $count) {
  $edge_count = rand_range(1, 103);
  $j = 0;
  while ($j < $edge_count) {
  $n = rand_range(0, $count);
  if ($n != $i) {
  g_add_pair($g, $i, $n, 1);
}
  $j = $j + 1;
};
  $i = $i + 1;
};
}
function g_dfs_time($g, $s, $e) {
  $begin = _now();
  g_dfs($g, $s, $e);
  $end = _now();
  return $end - $begin;
}
function g_bfs_time($g, $s) {
  $begin = _now();
  g_bfs($g, $s);
  $end = _now();
  return $end - $begin;
}
function main() {
  $dg = dg_make_graph();
  dg_add_pair($dg, 0, 1, 5);
  dg_add_pair($dg, 0, 2, 3);
  dg_add_pair($dg, 1, 3, 2);
  dg_add_pair($dg, 2, 3, 4);
  echo rtrim(_str(dg_dfs($dg, -2, -1))), PHP_EOL;
  echo rtrim(_str(dg_bfs($dg, -2))), PHP_EOL;
  echo rtrim(_str(dg_in_degree($dg, 3))), PHP_EOL;
  echo rtrim(_str(dg_out_degree($dg, 0))), PHP_EOL;
  echo rtrim(_str(dg_topological_sort($dg))), PHP_EOL;
  echo rtrim(_str(dg_has_cycle($dg))), PHP_EOL;
  $ug = g_make_graph();
  g_add_pair($ug, 0, 1, 1);
  g_add_pair($ug, 1, 2, 1);
  g_add_pair($ug, 2, 0, 1);
  echo rtrim(_str(g_dfs($ug, -2, -1))), PHP_EOL;
  echo rtrim(_str(g_bfs($ug, -2))), PHP_EOL;
  echo rtrim(_str(g_degree($ug, 1))), PHP_EOL;
  echo rtrim(_str(g_has_cycle($ug))), PHP_EOL;
}
main();
