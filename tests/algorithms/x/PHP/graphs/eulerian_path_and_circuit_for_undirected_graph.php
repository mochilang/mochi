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
function make_matrix($n) {
  global $g1, $g2, $g3, $g4, $g5, $max_node;
  $matrix = [];
  $i = 0;
  while ($i <= $n) {
  $row = [];
  $j = 0;
  while ($j <= $n) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $i = $i + 1;
};
  return $matrix;
}
function dfs($u, $graph, &$visited_edge, $path) {
  global $g1, $g2, $g3, $g4, $g5, $max_node;
  $path = _append($path, $u);
  if (array_key_exists($u, $graph)) {
  $neighbors = $graph[$u];
  $i = 0;
  while ($i < count($neighbors)) {
  $v = $neighbors[$i];
  if ($visited_edge[$u][$v] == false) {
  $visited_edge[$u][$v] = true;
  $visited_edge[$v][$u] = true;
  $path = dfs($v, $graph, $visited_edge, $path);
}
  $i = $i + 1;
};
}
  return $path;
}
function check_circuit_or_path($graph, $max_node) {
  global $g1, $g2, $g3, $g4, $g5;
  $odd_degree_nodes = 0;
  $odd_node = -1;
  $i = 0;
  while ($i < $max_node) {
  if (array_key_exists($i, $graph)) {
  if (fmod(count($graph[$i]), 2) == 1) {
  $odd_degree_nodes = $odd_degree_nodes + 1;
  $odd_node = $i;
};
}
  $i = $i + 1;
};
  if ($odd_degree_nodes == 0) {
  return ['status' => 1, 'odd_node' => $odd_node];
}
  if ($odd_degree_nodes == 2) {
  return ['status' => 2, 'odd_node' => $odd_node];
}
  return ['status' => 3, 'odd_node' => $odd_node];
}
function check_euler($graph, $max_node) {
  global $g1, $g2, $g3, $g4, $g5;
  $visited_edge = make_matrix($max_node);
  $res = check_circuit_or_path($graph, $max_node);
  if ($res['status'] == 3) {
  echo rtrim('graph is not Eulerian'), PHP_EOL;
  echo rtrim('no path'), PHP_EOL;
  return;
}
  $start_node = 1;
  if ($res['status'] == 2) {
  $start_node = $res['odd_node'];
  echo rtrim('graph has a Euler path'), PHP_EOL;
}
  if ($res['status'] == 1) {
  echo rtrim('graph has a Euler cycle'), PHP_EOL;
}
  $path = dfs($start_node, $graph, $visited_edge, []);
  echo rtrim(_str($path)), PHP_EOL;
}
$g1 = [1 => [2, 3, 4], 2 => [1, 3], 3 => [1, 2], 4 => [1, 5], 5 => [4]];
$g2 = [1 => [2, 3, 4, 5], 2 => [1, 3], 3 => [1, 2], 4 => [1, 5], 5 => [1, 4]];
$g3 = [1 => [2, 3, 4], 2 => [1, 3, 4], 3 => [1, 2], 4 => [1, 2, 5], 5 => [4]];
$g4 = [1 => [2, 3], 2 => [1, 3], 3 => [1, 2]];
$g5 = [1 => [], 2 => []];
$max_node = 10;
check_euler($g1, $max_node);
check_euler($g2, $max_node);
check_euler($g3, $max_node);
check_euler($g4, $max_node);
check_euler($g5, $max_node);
