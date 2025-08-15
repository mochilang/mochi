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
function connect($graph, $a, $b, $w) {
  global $INF;
  $u = $a - 1;
  $v = $b - 1;
  $g = $graph;
  $g[$u] = _append($g[$u], [$v, $w]);
  $g[$v] = _append($g[$v], [$u, $w]);
  return $g;
}
function in_list($arr, $x) {
  global $INF;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function prim($graph, $s, $n) {
  global $INF;
  $dist = [];
  $parent = [];
  $dist[$s] = 0;
  $parent[$s] = -1;
  $known = [];
  $keys = [$s];
  while (count($known) < $n) {
  $mini = $INF;
  $u = -1;
  $i = 0;
  while ($i < count($keys)) {
  $k = $keys[$i];
  $d = $dist[$k];
  if (!(in_list($known, $k)) && $d < $mini) {
  $mini = $d;
  $u = $k;
}
  $i = $i + 1;
};
  $known = _append($known, $u);
  foreach ($graph[$u] as $e) {
  $v = $e[0];
  $w = $e[1];
  if (!(in_list($keys, $v))) {
  $keys = _append($keys, $v);
}
  $cur = (array_key_exists($v, $dist) ? $dist[$v] : $INF);
  if (!(in_list($known, $v)) && $w < $cur) {
  $dist[$v] = $w;
  $parent[$v] = $u;
}
};
};
  $edges = [];
  $j = 0;
  while ($j < count($keys)) {
  $v = $keys[$j];
  if ($v != $s) {
  $edges = _append($edges, [$v + 1, $parent[$v] + 1]);
}
  $j = $j + 1;
};
  return $edges;
}
function sort_heap($h, $dist) {
  global $INF;
  $a = $h;
  $i = 0;
  while ($i < count($a)) {
  $j = 0;
  while ($j < count($a) - $i - 1) {
  $dj = (array_key_exists($a[$j], $dist) ? $dist[$a[$j]] : $INF);
  $dj1 = (array_key_exists($a[$j + 1], $dist) ? $dist[$a[$j + 1]] : $INF);
  if ($dj > $dj1) {
  $t = $a[$j];
  $a[$j] = $a[$j + 1];
  $a[$j + 1] = $t;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $a;
}
function prim_heap($graph, $s, $n) {
  global $INF;
  $dist = [];
  $parent = [];
  $dist[$s] = 0;
  $parent[$s] = -1;
  $h = [];
  $i = 0;
  while ($i < $n) {
  $h = _append($h, $i);
  $i = $i + 1;
};
  $h = sort_heap($h, $dist);
  $known = [];
  while (count($h) > 0) {
  $u = $h[0];
  $h = array_slice($h, 1, count($h) - 1);
  $known = _append($known, $u);
  foreach ($graph[$u] as $e) {
  $v = $e[0];
  $w = $e[1];
  $cur = (array_key_exists($v, $dist) ? $dist[$v] : $INF);
  if (!(in_list($known, $v)) && $w < $cur) {
  $dist[$v] = $w;
  $parent[$v] = $u;
}
};
  $h = sort_heap($h, $dist);
};
  $edges = [];
  $j = 0;
  while ($j < $n) {
  if ($j != $s) {
  $edges = _append($edges, [$j + 1, $parent[$j] + 1]);
}
  $j = $j + 1;
};
  return $edges;
}
function print_edges($edges) {
  global $INF;
  $i = 0;
  while ($i < count($edges)) {
  $e = $edges[$i];
  echo rtrim('(' . _str($e[0]) . ', ' . _str($e[1]) . ')'), PHP_EOL;
  $i = $i + 1;
};
}
function test_vector() {
  global $INF;
  $x = 5;
  $G = [];
  $i = 0;
  while ($i < $x) {
  $G[$i] = [];
  $i = $i + 1;
};
  $G = connect($G, 1, 2, 15);
  $G = connect($G, 1, 3, 12);
  $G = connect($G, 2, 4, 13);
  $G = connect($G, 2, 5, 5);
  $G = connect($G, 3, 2, 6);
  $G = connect($G, 3, 4, 6);
  $mst = prim($G, 0, $x);
  print_edges($mst);
  $mst_heap = prim_heap($G, 0, $x);
  print_edges($mst_heap);
}
test_vector();
