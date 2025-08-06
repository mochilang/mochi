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
function make_int_list($n, $value) {
  global $graph, $dist;
  $lst = [];
  $i = 0;
  while ($i < $n) {
  $lst = _append($lst, $value);
  $i = $i + 1;
};
  return $lst;
}
function make_bool_list($n) {
  global $graph, $dist;
  $lst = [];
  $i = 0;
  while ($i < $n) {
  $lst = _append($lst, false);
  $i = $i + 1;
};
  return $lst;
}
function dijkstra($graph, $src) {
  $n = count($graph);
  $dist = make_int_list($n, 1000000000);
  $visited = make_bool_list($n);
  $dist[$src] = 0;
  $count = 0;
  while ($count < $n) {
  $u = -1;
  $min_dist = 1000000000;
  $i = 0;
  while ($i < $n) {
  if (!$visited[$i] && $dist[$i] < $min_dist) {
  $min_dist = $dist[$i];
  $u = $i;
}
  $i = $i + 1;
};
  if ($u < 0) {
  break;
}
  $visited[$u] = true;
  $j = 0;
  while ($j < count($graph[$u])) {
  $e = $graph[$u][$j];
  $v = $e['node'];
  $w = $e['weight'];
  if (!$visited[$v]) {
  $new_dist = $dist[$u] + $w;
  if ($new_dist < $dist[$v]) {
  $dist[$v] = $new_dist;
};
}
  $j = $j + 1;
};
  $count = $count + 1;
};
  return $dist;
}
$graph = [[['node' => 1, 'weight' => 10], ['node' => 3, 'weight' => 5]], [['node' => 2, 'weight' => 1], ['node' => 3, 'weight' => 2]], [['node' => 4, 'weight' => 4]], [['node' => 1, 'weight' => 3], ['node' => 2, 'weight' => 9], ['node' => 4, 'weight' => 2]], [['node' => 0, 'weight' => 7], ['node' => 2, 'weight' => 6]]];
$dist = dijkstra($graph, 0);
echo rtrim(_str($dist[0])), PHP_EOL;
echo rtrim(_str($dist[1])), PHP_EOL;
echo rtrim(_str($dist[2])), PHP_EOL;
echo rtrim(_str($dist[3])), PHP_EOL;
echo rtrim(_str($dist[4])), PHP_EOL;
