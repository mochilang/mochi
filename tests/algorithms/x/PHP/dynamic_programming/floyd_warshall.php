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
function new_graph($n) {
  global $INF, $graph;
  $dp = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 0);
} else {
  $row = _append($row, $INF);
}
  $j = $j + 1;
};
  $dp = _append($dp, $row);
  $i = $i + 1;
};
  return ['n' => $n, 'dp' => $dp];
}
function add_edge(&$g, $u, $v, $w) {
  global $INF, $graph;
  $dp = $g['dp'];
  $row = $dp[$u];
  $row[$v] = $w;
  $dp[$u] = $row;
  $g['dp'] = $dp;
}
function floyd_warshall(&$g) {
  global $INF, $graph;
  $dp = $g['dp'];
  $k = 0;
  while ($k < $g['n']) {
  $i = 0;
  while ($i < $g['n']) {
  $j = 0;
  while ($j < $g['n']) {
  $alt = $dp[$i][$k] + $dp[$k][$j];
  $row = $dp[$i];
  if ($alt < $row[$j]) {
  $row[$j] = $alt;
  $dp[$i] = $row;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $k = $k + 1;
};
  $g['dp'] = $dp;
}
function show_min($g, $u, $v) {
  global $INF, $graph;
  return $g['dp'][$u][$v];
}
$graph = new_graph(5);
add_edge($graph, 0, 2, 9);
add_edge($graph, 0, 4, 10);
add_edge($graph, 1, 3, 5);
add_edge($graph, 2, 3, 7);
add_edge($graph, 3, 0, 10);
add_edge($graph, 3, 1, 2);
add_edge($graph, 3, 2, 1);
add_edge($graph, 3, 4, 6);
add_edge($graph, 4, 1, 3);
add_edge($graph, 4, 2, 4);
add_edge($graph, 4, 3, 9);
floyd_warshall($graph);
echo rtrim(_str(show_min($graph, 1, 4))), PHP_EOL;
echo rtrim(_str(show_min($graph, 0, 3))), PHP_EOL;
