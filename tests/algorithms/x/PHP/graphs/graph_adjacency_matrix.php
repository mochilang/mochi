<?php
ini_set('memory_limit', '-1');
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
function make_graph($vertices, $edges, $directed) {
  $g = ['directed' => $directed, 'vertex_to_index' => [], 'adj_matrix' => []];
  $i = 0;
  while ($i < count($vertices)) {
  add_vertex($g, $vertices[$i]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < count($edges)) {
  $e = $edges[$j];
  add_edge($g, $e[0], $e[1]);
  $j = $j + 1;
};
  return $g;
}
function contains_vertex($g, $v) {
  return isset($g['vertex_to_index'][$v]);
}
function add_vertex(&$g, $v) {
  if (contains_vertex($g, $v)) {
  $panic('vertex already exists');
}
  $matrix = $g['adj_matrix'];
  $i = 0;
  while ($i < count($matrix)) {
  $matrix[$i] = _append($matrix[$i], 0);
  $i = $i + 1;
};
  $row = [];
  $j = 0;
  while ($j < count($matrix) + 1) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $g['adj_matrix'] = $matrix;
  $idx_map = $g['vertex_to_index'];
  $idx_map[$v] = count($matrix) - 1;
  $g['vertex_to_index'] = $idx_map;
}
function remove_key($m, $k) {
  global $g;
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
}
function decrement_indices($m, $start) {
  global $g;
  $out = [];
  foreach (array_keys($m) as $key) {
  $idx = $m[$key];
  if ($idx > $start) {
  $out[$key] = $idx - 1;
} else {
  $out[$key] = $idx;
}
};
  return $out;
}
function remove_vertex(&$g, $v) {
  if (!contains_vertex($g, $v)) {
  $panic('vertex does not exist');
}
  $idx = ($g['vertex_to_index'])[$v];
  $new_matrix = [];
  $i = 0;
  while ($i < _len($g['adj_matrix'])) {
  if ($i != $idx) {
  $row = $g['adj_matrix'][$i];
  $new_row = [];
  $j = 0;
  while ($j < count($row)) {
  if ($j != $idx) {
  $new_row = _append($new_row, $row[$j]);
}
  $j = $j + 1;
};
  $new_matrix = _append($new_matrix, $new_row);
}
  $i = $i + 1;
};
  $g['adj_matrix'] = $new_matrix;
  $m = remove_key($g['vertex_to_index'], $v);
  $g['vertex_to_index'] = decrement_indices($m, $idx);
}
function add_edge(&$g, $u, $v) {
  if (!(contains_vertex($g, $u) && contains_vertex($g, $v))) {
  $panic('missing vertex');
}
  $i = ($g['vertex_to_index'])[$u];
  $j = ($g['vertex_to_index'])[$v];
  $matrix = $g['adj_matrix'];
  $matrix[$i][$j] = 1;
  if (!$g['directed']) {
  $matrix[$j][$i] = 1;
}
  $g['adj_matrix'] = $matrix;
}
function remove_edge(&$g, $u, $v) {
  if (!(contains_vertex($g, $u) && contains_vertex($g, $v))) {
  $panic('missing vertex');
}
  $i = ($g['vertex_to_index'])[$u];
  $j = ($g['vertex_to_index'])[$v];
  $matrix = $g['adj_matrix'];
  $matrix[$i][$j] = 0;
  if (!$g['directed']) {
  $matrix[$j][$i] = 0;
}
  $g['adj_matrix'] = $matrix;
}
function contains_edge($g, $u, $v) {
  if (!(contains_vertex($g, $u) && contains_vertex($g, $v))) {
  $panic('missing vertex');
}
  $i = ($g['vertex_to_index'])[$u];
  $j = ($g['vertex_to_index'])[$v];
  $matrix = $g['adj_matrix'];
  return $matrix[$i][$j] == 1;
}
function clear_graph(&$g) {
  $g['vertex_to_index'] = [];
  $g['adj_matrix'] = [];
}
$g = make_graph([1, 2, 3], [[1, 2], [2, 3]], false);
echo rtrim(_str($g['adj_matrix'])), PHP_EOL;
echo rtrim(_str(contains_edge($g, 1, 2))), PHP_EOL;
echo rtrim(_str(contains_edge($g, 2, 1))), PHP_EOL;
remove_edge($g, 1, 2);
echo rtrim(_str(contains_edge($g, 1, 2))), PHP_EOL;
remove_vertex($g, 2);
echo rtrim(_str($g['adj_matrix'])), PHP_EOL;
