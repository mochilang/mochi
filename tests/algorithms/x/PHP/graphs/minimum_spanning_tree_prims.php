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
$INF = 1000000000;
function pairs_to_string($edges) {
  global $INF, $adjacency_list, $mst_edges;
  $s = '[';
  $i = 0;
  while ($i < count($edges)) {
  $e = $edges[$i];
  $s = $s . '(' . _str($e['u']) . ', ' . _str($e['v']) . ')';
  if ($i < count($edges) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function prim_mst($graph) {
  global $INF, $adjacency_list, $mst_edges;
  $n = count($graph);
  $visited = [];
  $dist = [];
  $parent = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $dist = _append($dist, $INF);
  $parent = _append($parent, -1);
  $i = $i + 1;
};
  $dist[0] = 0;
  $result = [];
  $count = 0;
  while ($count < $n) {
  $min_val = $INF;
  $u = 0;
  $v = 0;
  while ($v < $n) {
  if ($visited[$v] == false && $dist[$v] < $min_val) {
  $min_val = $dist[$v];
  $u = $v;
}
  $v = $v + 1;
};
  if ($min_val == $INF) {
  break;
}
  $visited[$u] = true;
  if ($u != 0) {
  $result = _append($result, [$u => $parent[$u], $v => $u]);
}
  foreach ($graph[$u] as $e) {
  if ($visited[$e['to']] == false && $e['weight'] < $dist[$e['to']]) {
  $dist[$e['to']] = $e['weight'];
  $parent[$e['to']] = $u;
}
};
  $count = $count + 1;
};
  return $result;
}
$adjacency_list = [[['to' => 1, 'weight' => 1], ['to' => 3, 'weight' => 3]], [['to' => 0, 'weight' => 1], ['to' => 2, 'weight' => 6], ['to' => 3, 'weight' => 5], ['to' => 4, 'weight' => 1]], [['to' => 1, 'weight' => 6], ['to' => 4, 'weight' => 5], ['to' => 5, 'weight' => 2]], [['to' => 0, 'weight' => 3], ['to' => 1, 'weight' => 5], ['to' => 4, 'weight' => 1]], [['to' => 1, 'weight' => 1], ['to' => 2, 'weight' => 5], ['to' => 3, 'weight' => 1], ['to' => 5, 'weight' => 4]], [['to' => 2, 'weight' => 2], ['to' => 4, 'weight' => 4]]];
$mst_edges = prim_mst($adjacency_list);
echo rtrim(pairs_to_string($mst_edges)), PHP_EOL;
