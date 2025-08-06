<?php
ini_set('memory_limit', '-1');
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
function prims_algo($graph) {
  global $res;
  $INF = 2147483647;
  $dist = [];
  $parent = [];
  $queue = [];
  foreach (array_keys($graph) as $node) {
  $dist[$node] = $INF;
  $parent[$node] = '';
  $queue = _append($queue, [$node => $node, 'weight' => $INF]);
};
  if (count($queue) == 0) {
  return ['dist' => $dist, 'parent' => $parent];
}
  $min_idx = 0;
  $i = 1;
  while ($i < count($queue)) {
  if ($queue[$i]['weight'] < $queue[$min_idx]['weight']) {
  $min_idx = $i;
}
  $i = $i + 1;
};
  $start_node = $queue[$min_idx];
  $start = $start_node['node'];
  $new_q = [];
  $j = 0;
  while ($j < count($queue)) {
  if ($j != $min_idx) {
  $new_q = _append($new_q, $queue[$j]);
}
  $j = $j + 1;
};
  $queue = $new_q;
  $dist[$start] = 0;
  foreach (array_keys($graph[$start]) as $neighbour) {
  $w = $graph[$start][$neighbour];
  if ($dist[$neighbour] > $dist[$start] + $w) {
  $dist[$neighbour] = $dist[$start] + $w;
  $parent[$neighbour] = $start;
  $k = 0;
  while ($k < count($queue)) {
  if ($queue[$k]['node'] == $neighbour) {
  $queue[$k]['weight'] = $dist[$neighbour];
  break;
}
  $k = $k + 1;
};
}
};
  while (count($queue) > 0) {
  $best_idx = 0;
  $p = 1;
  while ($p < count($queue)) {
  if ($queue[$p]['weight'] < $queue[$best_idx]['weight']) {
  $best_idx = $p;
}
  $p = $p + 1;
};
  $node_entry = $queue[$best_idx];
  $node = $node_entry['node'];
  $tmp = [];
  $q = 0;
  while ($q < count($queue)) {
  if ($q != $best_idx) {
  $tmp = _append($tmp, $queue[$q]);
}
  $q = $q + 1;
};
  $queue = $tmp;
  foreach (array_keys($graph[$node]) as $neighbour) {
  $w = $graph[$node][$neighbour];
  if ($dist[$neighbour] > $dist[$node] + $w) {
  $dist[$neighbour] = $dist[$node] + $w;
  $parent[$neighbour] = $node;
  $r = 0;
  while ($r < count($queue)) {
  if ($queue[$r]['node'] == $neighbour) {
  $queue[$r]['weight'] = $dist[$neighbour];
  break;
}
  $r = $r + 1;
};
}
};
};
  return ['dist' => $dist, 'parent' => $parent];
}
function iabs($x) {
  global $graph, $res, $dist;
  if ($x < 0) {
  return -$x;
}
  return $x;
}
$graph = [];
$graph['a'] = ['b' => 3, 'c' => 15];
$graph['b'] = ['a' => 3, 'c' => 10, 'd' => 100];
$graph['c'] = ['a' => 15, 'b' => 10, 'd' => 5];
$graph['d'] = ['b' => 100, 'c' => 5];
$res = prims_algo($graph);
$dist = $res['dist'];
echo rtrim(_str(iabs($dist['a'] - $dist['b']))), PHP_EOL;
echo rtrim(_str(iabs($dist['d'] - $dist['b']))), PHP_EOL;
echo rtrim(_str(iabs($dist['a'] - $dist['c']))), PHP_EOL;
