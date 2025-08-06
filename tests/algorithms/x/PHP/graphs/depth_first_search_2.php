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
function add_edge(&$g, $from_vertex, $to_vertex) {
  $v = $g['vertex'];
  if (array_key_exists($from_vertex, $v)) {
  $lst = $v[$from_vertex];
  $lst = _append($lst, $to_vertex);
  $v[$from_vertex] = $lst;
} else {
  $v[$from_vertex] = [$to_vertex];
}
  $g['vertex'] = $v;
  if ($from_vertex + 1 > $g['size']) {
  $g['size'] = $from_vertex + 1;
}
  if ($to_vertex + 1 > $g['size']) {
  $g['size'] = $to_vertex + 1;
}
  return $g;
}
function list_to_string($lst) {
  global $g;
  $res = '';
  $i = 0;
  while ($i < count($lst)) {
  $res = $res . _str($lst[$i]);
  if ($i < count($lst) - 1) {
  $res = $res . ' ';
}
  $i = $i + 1;
};
  return $res;
}
function list_to_arrow($lst) {
  global $g;
  $res = '';
  $i = 0;
  while ($i < count($lst)) {
  $res = $res . _str($lst[$i]);
  if ($i < count($lst) - 1) {
  $res = $res . ' -> ';
}
  $i = $i + 1;
};
  return $res;
}
function print_graph($g) {
  echo rtrim(_str($g['vertex'])), PHP_EOL;
  $i = 0;
  while ($i < $g['size']) {
  $edges = [];
  if (isset($g['vertex'][$i])) {
  $edges = $g['vertex'][$i];
}
  $line = _str($i) . '  ->  ' . list_to_arrow($edges);
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
}
function dfs_recursive($g, $start_vertex, &$visited, $order) {
  $visited[$start_vertex] = true;
  $order = _append($order, $start_vertex);
  if (isset($g['vertex'][$start_vertex])) {
  $neighbors = $g['vertex'][$start_vertex];
  $i = 0;
  while ($i < count($neighbors)) {
  $nb = $neighbors[$i];
  if (!$visited[$nb]) {
  $order = dfs_recursive($g, $nb, $visited, $order);
}
  $i = $i + 1;
};
}
  return $order;
}
function dfs($g) {
  $n = $g['size'];
  $visited = [];
  $i = 0;
  while ($i < $n) {
  $visited = _append($visited, false);
  $i = $i + 1;
};
  $order = [];
  $i = 0;
  while ($i < $n) {
  if (!$visited[$i]) {
  $order = dfs_recursive($g, $i, $visited, $order);
}
  $i = $i + 1;
};
  return $order;
}
$g = ['vertex' => [], 'size' => 0];
$g = add_edge($g, 0, 1);
$g = add_edge($g, 0, 2);
$g = add_edge($g, 1, 2);
$g = add_edge($g, 2, 0);
$g = add_edge($g, 2, 3);
$g = add_edge($g, 3, 3);
print_graph($g);
echo rtrim('DFS:'), PHP_EOL;
echo rtrim(list_to_string(dfs($g))), PHP_EOL;
