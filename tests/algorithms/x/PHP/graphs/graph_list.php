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
function make_graph($directed) {
  global $d_graph;
  $m = [];
  return ['adj_list' => $m, 'directed' => $directed];
}
function contains_vertex($m, $v) {
  global $d_graph;
  return array_key_exists($v, $m);
}
function add_edge(&$g, $s, $d) {
  global $d_graph;
  $adj = $g['adj_list'];
  if (!$g['directed']) {
  if (contains_vertex($adj, $s) && contains_vertex($adj, $d)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = _append($adj[$d], $s);
} else {
  if (contains_vertex($adj, $s)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = [$s];
} else {
  if (contains_vertex($adj, $d)) {
  $adj[$d] = _append($adj[$d], $s);
  $adj[$s] = [$d];
} else {
  $adj[$s] = [$d];
  $adj[$d] = [$s];
};
};
};
} else {
  if (contains_vertex($adj, $s) && contains_vertex($adj, $d)) {
  $adj[$s] = _append($adj[$s], $d);
} else {
  if (contains_vertex($adj, $s)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = [];
} else {
  if (contains_vertex($adj, $d)) {
  $adj[$s] = [$d];
} else {
  $adj[$s] = [$d];
  $adj[$d] = [];
};
};
};
}
  $g['adj_list'] = $adj;
  return $g;
}
function graph_to_string($g) {
  global $d_graph;
  return _str($g['adj_list']);
}
$d_graph = make_graph(true);
$d_graph = add_edge($d_graph, _str(0), _str(1));
echo rtrim(graph_to_string($d_graph)), PHP_EOL;
$d_graph = add_edge($d_graph, _str(1), _str(2));
$d_graph = add_edge($d_graph, _str(1), _str(4));
$d_graph = add_edge($d_graph, _str(1), _str(5));
echo rtrim(graph_to_string($d_graph)), PHP_EOL;
$d_graph = add_edge($d_graph, _str(2), _str(0));
$d_graph = add_edge($d_graph, _str(2), _str(6));
$d_graph = add_edge($d_graph, _str(2), _str(7));
echo rtrim(graph_to_string($d_graph)), PHP_EOL;
