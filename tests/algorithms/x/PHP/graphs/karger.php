<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$seed = 1;
function rand_int($n) {
  global $seed, $TEST_GRAPH, $result;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed % $n;
}
function contains($list, $value) {
  global $seed, $TEST_GRAPH, $result;
  $i = 0;
  while ($i < count($list)) {
  if ($list[$i] == $value) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function remove_all($list, $value) {
  global $seed, $TEST_GRAPH, $result;
  $res = [];
  $i = 0;
  while ($i < count($list)) {
  if ($list[$i] != $value) {
  $res = _append($res, $list[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function partition_graph($graph) {
  global $seed, $TEST_GRAPH, $result;
  $contracted = [];
  foreach (array_keys($graph) as $node) {
  $contracted[$node] = [$node];
};
  $graph_copy = [];
  foreach (array_keys($graph) as $node) {
  $lst = [];
  $neigh = $graph[$node];
  $i = 0;
  while ($i < count($neigh)) {
  $lst = _append($lst, $neigh[$i]);
  $i = $i + 1;
};
  $graph_copy[$node] = $lst;
};
  $nodes = array_keys($graph_copy);
  while (count($nodes) > 2) {
  $u = $nodes[rand_int(count($nodes))];
  $u_neighbors = $graph_copy[$u];
  $v = $u_neighbors[rand_int(count($u_neighbors))];
  $uv = $u . $v;
  $uv_neighbors = [];
  $i = 0;
  while ($i < count($graph_copy[$u])) {
  $n = $graph_copy[$u][$i];
  if ($n != $u && $n != $v && contains($uv_neighbors, $n) == false) {
  $uv_neighbors = _append($uv_neighbors, $n);
}
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($graph_copy[$v])) {
  $n = $graph_copy[$v][$i];
  if ($n != $u && $n != $v && contains($uv_neighbors, $n) == false) {
  $uv_neighbors = _append($uv_neighbors, $n);
}
  $i = $i + 1;
};
  $graph_copy[$uv] = $uv_neighbors;
  $k = 0;
  while ($k < count($uv_neighbors)) {
  $nb = $uv_neighbors[$k];
  $graph_copy[$nb] = _append($graph_copy[$nb], $uv);
  $graph_copy[$nb] = remove_all($graph_copy[$nb], $u);
  $graph_copy[$nb] = remove_all($graph_copy[$nb], $v);
  $k = $k + 1;
};
  $group = [];
  $i = 0;
  while ($i < count($contracted[$u])) {
  $group = _append($group, $contracted[$u][$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($contracted[$v])) {
  $val = $contracted[$v][$i];
  if (contains($group, $val) == false) {
  $group = _append($group, $val);
}
  $i = $i + 1;
};
  $contracted[$uv] = $group;
  $nodes = remove_all($nodes, $u);
  $nodes = remove_all($nodes, $v);
  $nodes = _append($nodes, $uv);
};
  $groups = [];
  $j = 0;
  while ($j < count($nodes)) {
  $n = $nodes[$j];
  $groups = _append($groups, $contracted[$n]);
  $j = $j + 1;
};
  $groupA = $groups[0];
  $groupB = $groups[1];
  $cut = [];
  $j = 0;
  while ($j < count($groupA)) {
  $node = $groupA[$j];
  $neigh = $graph[$node];
  $l = 0;
  while ($l < count($neigh)) {
  $nb = $neigh[$l];
  if (contains($groupB, $nb)) {
  $cut = _append($cut, ['a' => $node, 'b' => $nb]);
}
  $l = $l + 1;
};
  $j = $j + 1;
};
  return $cut;
}
function cut_to_string($cut) {
  global $seed, $TEST_GRAPH, $result;
  $s = '{';
  $i = 0;
  while ($i < count($cut)) {
  $p = $cut[$i];
  $s = $s . '(' . $p['a'] . ', ' . $p['b'] . ')';
  if ($i < count($cut) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . '}';
  return $s;
}
$TEST_GRAPH = ['1' => ['2', '3', '4', '5'], '2' => ['1', '3', '4', '5'], '3' => ['1', '2', '4', '5', '10'], '4' => ['1', '2', '3', '5', '6'], '5' => ['1', '2', '3', '4', '7'], '6' => ['7', '8', '9', '10', '4'], '7' => ['6', '8', '9', '10', '5'], '8' => ['6', '7', '9', '10'], '9' => ['6', '7', '8', '10'], '10' => ['6', '7', '8', '9', '3']];
$result = partition_graph($TEST_GRAPH);
echo rtrim(cut_to_string($result)), PHP_EOL;
