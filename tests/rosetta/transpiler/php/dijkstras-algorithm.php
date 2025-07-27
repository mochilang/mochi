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
$INF = 1000000000;
$graph = [];
function addEdge($u, $v, $w) {
  global $INF, $graph;
  if (!(array_key_exists($u, $graph))) {
  $graph[$u] = [];
}
  $graph[$u][$v] = $w;
  if (!(array_key_exists($v, $graph))) {
  $graph[$v] = [];
}
}
function removeAt($xs, $idx) {
  global $INF, $graph;
  $out = [];
  $i = 0;
  foreach ($xs as $x) {
  if ($i != $idx) {
  $out = array_merge($out, [$x]);
}
  $i = $i + 1;
};
  return $out;
}
function dijkstra($source) {
  global $INF, $graph;
  $dist = [];
  $prev = [];
  foreach (array_keys($graph) as $v) {
  $dist[$v] = $INF;
  $prev[$v] = '';
};
  $dist[$source] = 0;
  $q = [];
  foreach (array_keys($graph) as $v) {
  $q = array_merge($q, [$v]);
};
  while (count($q) > 0) {
  $bestIdx = 0;
  $u = $q[0];
  $i = 1;
  while ($i < count($q)) {
  $v = $q[$i];
  if ($dist[$v] < $dist[$u]) {
  $u = $v;
  $bestIdx = $i;
}
  $i = $i + 1;
};
  $q = removeAt($q, $bestIdx);
  foreach (array_keys($graph[$u]) as $v) {
  $alt = $dist[$u] + $graph[$u][$v];
  if ($alt < $dist[$v]) {
  $dist[$v] = $alt;
  $prev[$v] = $u;
}
};
};
  return ['dist' => $dist, 'prev' => $prev];
}
function path($prev, $v) {
  global $INF, $graph;
  $s = $v;
  $cur = $v;
  while ($prev[$cur] != '') {
  $cur = $prev[$cur];
  $s = $cur . $s;
};
  return $s;
}
function main() {
  global $INF, $graph;
  addEdge('a', 'b', 7);
  addEdge('a', 'c', 9);
  addEdge('a', 'f', 14);
  addEdge('b', 'c', 10);
  addEdge('b', 'd', 15);
  addEdge('c', 'd', 11);
  addEdge('c', 'f', 2);
  addEdge('d', 'e', 6);
  addEdge('e', 'f', 9);
  $res = dijkstra('a');
  $dist = $res['dist'];
  $prev = $res['prev'];
  echo rtrim('Distance to e: ' . _str($dist['e']) . ', Path: ' . path($prev, 'e')), PHP_EOL;
  echo rtrim('Distance to f: ' . _str($dist['f']) . ', Path: ' . path($prev, 'f')), PHP_EOL;
}
main();
