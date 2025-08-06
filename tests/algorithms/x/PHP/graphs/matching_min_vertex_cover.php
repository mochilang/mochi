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
function contains($xs, $v) {
  global $graph, $cover;
  foreach ($xs as $x) {
  if ($x == $v) {
  return true;
}
};
  return false;
}
function get_edges($graph) {
  global $cover;
  $n = count($graph);
  $edges = [];
  for ($i = 0; $i < $n; $i++) {
  foreach ($graph[$i] as $j) {
  $edges = _append($edges, [$i, $j]);
};
};
  return $edges;
}
function matching_min_vertex_cover($graph) {
  global $cover;
  $chosen = [];
  $edges = get_edges($graph);
  while (count($edges) > 0) {
  $idx = count($edges) - 1;
  $e = $edges[$idx];
  $edges = array_slice($edges, 0, $idx - 0);
  $u = $e[0];
  $v = $e[1];
  if (!contains($chosen, $u)) {
  $chosen = _append($chosen, $u);
}
  if (!contains($chosen, $v)) {
  $chosen = _append($chosen, $v);
}
  $filtered = [];
  foreach ($edges as $edge) {
  $a = $edge[0];
  $b = $edge[1];
  if ($a != $u && $b != $u && $a != $v && $b != $v) {
  $filtered = _append($filtered, $edge);
}
};
  $edges = $filtered;
};
  return $chosen;
}
$graph = [0 => [1, 3], 1 => [0, 3], 2 => [0, 3, 4], 3 => [0, 1, 2], 4 => [2, 3]];
$cover = matching_min_vertex_cover($graph);
echo rtrim(_str($cover)), PHP_EOL;
