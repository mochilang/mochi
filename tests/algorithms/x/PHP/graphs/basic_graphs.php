<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function dfs($g, $s) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $visited = [];
  $stack = [];
  $visited[$s] = true;
  $stack = _append($stack, $s);
  echo rtrim(json_encode($s, 1344)), PHP_EOL;
  while (count($stack) > 0) {
  $u = $stack[count($stack) - 1];
  $found = false;
  foreach ($g[$u] as $v) {
  if (!(array_key_exists($v, $visited))) {
  $visited[$v] = true;
  $stack = _append($stack, $v);
  echo rtrim(json_encode($v, 1344)), PHP_EOL;
  $found = true;
  break;
}
};
  if (!$found) {
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
}
};
}
function bfs($g, $s) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $visited = [];
  $q = [];
  $visited[$s] = true;
  $q = _append($q, $s);
  echo rtrim(json_encode($s, 1344)), PHP_EOL;
  while (count($q) > 0) {
  $u = $q[0];
  $q = array_slice($q, 1, count($q) - 1);
  foreach ($g[$u] as $v) {
  if (!(array_key_exists($v, $visited))) {
  $visited[$v] = true;
  $q = _append($q, $v);
  echo rtrim(json_encode($v, 1344)), PHP_EOL;
}
};
};
}
function sort_ints($a) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $arr = $a;
  $i = 0;
  while ($i < count($arr)) {
  $j = 0;
  while ($j < count($arr) - $i - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function dijkstra($g, $s) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $dist = [];
  $dist[$s] = 0;
  $path = [];
  $path[$s] = 0;
  $known = [];
  $keys = [$s];
  while (count($known) < count($keys)) {
  $mini = 100000;
  $u = -1;
  $i = 0;
  while ($i < count($keys)) {
  $k = $keys[$i];
  $d = $dist[$k];
  if (!(in_array($k, $known)) && $d < $mini) {
  $mini = $d;
  $u = $k;
}
  $i = $i + 1;
};
  $known = _append($known, $u);
  foreach ($g[$u] as $e) {
  $v = $e[0];
  $w = $e[1];
  if (!(in_array($v, $keys))) {
  $keys = _append($keys, $v);
}
  $alt = $dist[$u] + $w;
  $cur = (array_key_exists($v, $dist) ? $dist[$v] : 100000);
  if (!(in_array($v, $known)) && $alt < $cur) {
  $dist[$v] = $alt;
  $path[$v] = $u;
}
};
};
  $ordered = sort_ints($keys);
  $idx = 0;
  while ($idx < count($ordered)) {
  $k = $ordered[$idx];
  if ($k != $s) {
  echo rtrim(json_encode($dist[$k], 1344)), PHP_EOL;
}
  $idx = $idx + 1;
};
}
function topo($g, $n) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $ind = [];
  $i = 0;
  while ($i <= $n) {
  $ind = _append($ind, 0);
  $i = $i + 1;
};
  $node = 1;
  while ($node <= $n) {
  foreach ($g[$node] as $v) {
  $ind[$v] = $ind[$v] + 1;
};
  $node = $node + 1;
};
  $q = [];
  $j = 1;
  while ($j <= $n) {
  if ($ind[$j] == 0) {
  $q = _append($q, $j);
}
  $j = $j + 1;
};
  while (count($q) > 0) {
  $v = $q[0];
  $q = array_slice($q, 1, count($q) - 1);
  echo rtrim(json_encode($v, 1344)), PHP_EOL;
  foreach ($g[$v] as $w) {
  $ind[$w] = $ind[$w] - 1;
  if ($ind[$w] == 0) {
  $q = _append($q, $w);
}
};
};
}
function floyd($a) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $n = count($a);
  $dist = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $a[$i][$j]);
  $j = $j + 1;
};
  $dist = _append($dist, $row);
  $i = $i + 1;
};
  $k = 0;
  while ($k < $n) {
  $ii = 0;
  while ($ii < $n) {
  $jj = 0;
  while ($jj < $n) {
  if ($dist[$ii][$jj] > $dist[$ii][$k] + $dist[$k][$jj]) {
  $dist[$ii][$jj] = $dist[$ii][$k] + $dist[$k][$jj];
}
  $jj = $jj + 1;
};
  $ii = $ii + 1;
};
  $k = $k + 1;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($dist, 1344)))))), PHP_EOL;
}
function prim($g, $s, $n) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $dist = [];
  $dist[$s] = 0;
  $known = [];
  $keys = [$s];
  $total = 0;
  while (count($known) < $n) {
  $mini = 100000;
  $u = -1;
  $i = 0;
  while ($i < count($keys)) {
  $k = $keys[$i];
  $d = $dist[$k];
  if (!(in_array($k, $known)) && $d < $mini) {
  $mini = $d;
  $u = $k;
}
  $i = $i + 1;
};
  $known = _append($known, $u);
  $total = $total + $mini;
  foreach ($g[$u] as $e) {
  $v = $e[0];
  $w = $e[1];
  if (!(in_array($v, $keys))) {
  $keys = _append($keys, $v);
}
  $cur = (array_key_exists($v, $dist) ? $dist[$v] : 100000);
  if (!(in_array($v, $known)) && $w < $cur) {
  $dist[$v] = $w;
}
};
};
  return $total;
}
function sort_edges($edges) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
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
}
function find_parent($parent, $x) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $r = $x;
  while ($parent[$r] != $r) {
  $r = $parent[$r];
};
  return $r;
}
function union_parent(&$parent, $a, $b) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $parent[$a] = $b;
}
function kruskal($edges, $n) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $es = sort_edges($edges);
  $parent = [];
  $i = 0;
  while ($i <= $n) {
  $parent = _append($parent, $i);
  $i = $i + 1;
};
  $total = 0;
  $count = 0;
  $idx = 0;
  while ($count < $n - 1 && $idx < count($es)) {
  $e = $es[$idx];
  $idx = $idx + 1;
  $u = $e[0];
  $v = $e[1];
  $w = $e[2];
  $ru = find_parent($parent, $u);
  $rv = find_parent($parent, $v);
  if ($ru != $rv) {
  union_parent($parent, $ru, $rv);
  $total = $total + $w;
  $count = $count + 1;
}
};
  return $total;
}
function find_isolated_nodes($g, $nodes) {
  global $g_dfs, $g_bfs, $g_weighted, $g_topo, $matrix, $g_prim, $edges_kruskal, $g_iso, $iso;
  $isolated = [];
  foreach ($nodes as $node) {
  if (count($g[$node]) == 0) {
  $isolated = _append($isolated, $node);
}
};
  return $isolated;
}
$g_dfs = [1 => [2, 3], 2 => [4, 5], 3 => [], 4 => [], 5 => []];
$g_bfs = [1 => [2, 3], 2 => [4, 5], 3 => [6, 7], 4 => [], 5 => [8], 6 => [], 7 => [], 8 => []];
$g_weighted = [1 => [[2, 7], [3, 9], [6, 14]], 2 => [[1, 7], [3, 10], [4, 15]], 3 => [[1, 9], [2, 10], [4, 11], [6, 2]], 4 => [[2, 15], [3, 11], [5, 6]], 5 => [[4, 6], [6, 9]], 6 => [[1, 14], [3, 2], [5, 9]]];
$g_topo = [1 => [2, 3], 2 => [4], 3 => [4], 4 => []];
$matrix = [[0, 5, 9, 100000], [100000, 0, 2, 8], [100000, 100000, 0, 7], [4, 100000, 100000, 0]];
$g_prim = [1 => [[2, 1], [3, 3]], 2 => [[1, 1], [3, 1], [4, 6]], 3 => [[1, 3], [2, 1], [4, 2]], 4 => [[2, 6], [3, 2]]];
$edges_kruskal = [[1, 2, 1], [2, 3, 2], [1, 3, 2], [3, 4, 1]];
$g_iso = [1 => [2, 3], 2 => [1, 3], 3 => [1, 2], 4 => []];
dfs($g_dfs, 1);
bfs($g_bfs, 1);
dijkstra($g_weighted, 1);
topo($g_topo, 4);
floyd($matrix);
echo rtrim(json_encode(prim($g_prim, 1, 4), 1344)), PHP_EOL;
echo rtrim(json_encode(kruskal($edges_kruskal, 4), 1344)), PHP_EOL;
$iso = find_isolated_nodes($g_iso, [1, 2, 3, 4]);
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($iso, 1344)))))), PHP_EOL;
