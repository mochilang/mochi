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
function create_graph($vertices, $edges, $directed) {
  $adj = [];
  foreach ($vertices as $v) {
  $adj[$v] = [];
};
  foreach ($edges as $e) {
  $s = $e[0];
  $d = $e[1];
  if (!(array_key_exists($s, $adj))) {
  $adj[$s] = [];
}
  if (!(array_key_exists($d, $adj))) {
  $adj[$d] = [];
}
  $adj[$s] = _append($adj[$s], $d);
  if (!$directed) {
  $adj[$d] = _append($adj[$d], $s);
}
};
  return ['adj' => $adj, 'directed' => $directed];
}
function add_vertex($graph, $v) {
  if (isset($graph['adj'][$v])) {
  $panic('vertex exists');
}
  $adj = [];
  foreach (array_keys($graph['adj']) as $k) {
  $adj[$k] = $graph['adj'][$k];
};
  $adj[$v] = [];
  return ['adj' => $adj, 'directed' => $graph['directed']];
}
function remove_from_list($lst, $value) {
  $res = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($lst[$i] != $value) {
  $res = _append($res, $lst[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function remove_key($m, $key) {
  $res = [];
  foreach (array_keys($m) as $k) {
  if ($k != $key) {
  $res[$k] = $m[$k];
}
};
  return $res;
}
function add_edge($graph, $s, $d) {
  if (((!(isset($graph['adj'][$s]))) || (!(isset($graph['adj'][$d]))))) {
  $panic('vertex missing');
}
  if (contains_edge($graph, $s, $d)) {
  $panic('edge exists');
}
  $adj = [];
  foreach (array_keys($graph['adj']) as $k) {
  $adj[$k] = $graph['adj'][$k];
};
  $list_s = $adj[$s];
  $list_s = _append($list_s, $d);
  $adj[$s] = $list_s;
  if (!$graph['directed']) {
  $list_d = $adj[$d];
  $list_d = _append($list_d, $s);
  $adj[$d] = $list_d;
}
  return ['adj' => $adj, 'directed' => $graph['directed']];
}
function remove_edge($graph, $s, $d) {
  if (((!(isset($graph['adj'][$s]))) || (!(isset($graph['adj'][$d]))))) {
  $panic('vertex missing');
}
  if (!contains_edge($graph, $s, $d)) {
  $panic('edge missing');
}
  $adj = [];
  foreach (array_keys($graph['adj']) as $k) {
  $adj[$k] = $graph['adj'][$k];
};
  $adj[$s] = remove_from_list($adj[$s], $d);
  if (!$graph['directed']) {
  $adj[$d] = remove_from_list($adj[$d], $s);
}
  return ['adj' => $adj, 'directed' => $graph['directed']];
}
function remove_vertex($graph, $v) {
  if (!(isset($graph['adj'][$v]))) {
  $panic('vertex missing');
}
  $adj = [];
  foreach (array_keys($graph['adj']) as $k) {
  if ($k != $v) {
  $adj[$k] = remove_from_list($graph['adj'][$k], $v);
}
};
  return ['adj' => $adj, 'directed' => $graph['directed']];
}
function contains_vertex($graph, $v) {
  return isset($graph['adj'][$v]);
}
function contains_edge($graph, $s, $d) {
  if (((!(isset($graph['adj'][$s]))) || (!(isset($graph['adj'][$d]))))) {
  $panic('vertex missing');
}
  foreach ($graph['adj'][$s] as $x) {
  if ($x == $d) {
  return true;
}
};
  return false;
}
function clear_graph($graph) {
  return ['adj' => [], 'directed' => $graph['directed']];
}
function to_string($graph) {
  return _str($graph['adj']);
}
function main() {
  $vertices = ['1', '2', '3', '4'];
  $edges = [['1', '2'], ['2', '3'], ['3', '4']];
  $g = create_graph($vertices, $edges, false);
  echo rtrim(to_string($g)), PHP_EOL;
  $g = add_vertex($g, '5');
  $g = add_edge($g, '4', '5');
  echo rtrim(_str(contains_edge($g, '4', '5'))), PHP_EOL;
  $g = remove_edge($g, '1', '2');
  $g = remove_vertex($g, '3');
  echo rtrim(to_string($g)), PHP_EOL;
}
main();
