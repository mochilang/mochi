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
function pow2($k) {
  global $INF, $graph, $source, $sink, $v;
  $res = 1;
  $i = 0;
  while ($i < $k) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
}
function min2($a, $b) {
  global $INF, $graph, $source, $sink, $v;
  if ($a < $b) {
  return $a;
}
  return $b;
}
function new_dinic($n) {
  global $INF, $graph, $source, $sink, $v;
  $lvl = [];
  $ptr = [];
  $q = [];
  $adj = [];
  $i = 0;
  while ($i < $n) {
  $lvl = _append($lvl, 0);
  $ptr = _append($ptr, 0);
  $q = _append($q, 0);
  $edges = [];
  $adj = _append($adj, $edges);
  $i = $i + 1;
};
  return ['n' => $n, 'lvl' => $lvl, 'ptr' => $ptr, 'q' => $q, 'adj' => $adj];
}
function add_edge(&$g, $a, $b, $c, $rcap) {
  global $INF, $graph, $source, $sink, $v;
  $adj = $g['adj'];
  $list_a = $adj[$a];
  $list_b = $adj[$b];
  $e1 = [$b, count($list_b), $c, 0];
  $e2 = [$a, count($list_a), $rcap, 0];
  $list_a = _append($list_a, $e1);
  $list_b = _append($list_b, $e2);
  $adj[$a] = $list_a;
  $adj[$b] = $list_b;
  $g['adj'] = $adj;
}
function dfs(&$g, $v, $sink, $flow) {
  global $INF, $graph, $source;
  if ($v == $sink || $flow == 0) {
  return $flow;
}
  $ptr = $g['ptr'];
  $i = $ptr[$v];
  $adj_all = $g['adj'];
  $adj_v = $adj_all[$v];
  while ($i < count($adj_v)) {
  $e = $adj_v[$i];
  $to = $e[0];
  if ($g['lvl'][$to] == $g['lvl'][$v] + 1) {
  $avail = $e[2] - $e[3];
  $pushed = dfs($g, $to, $sink, min2($flow, $avail));
  if ($pushed > 0) {
  $e[3] = $e[3] + $pushed;
  $adj_v[$i] = $e;
  $adj_to = $adj_all[$to];
  $back = $adj_to[$e[1]];
  $back[3] = $back[3] - $pushed;
  $adj_to[$e[1]] = $back;
  $adj_all[$to] = $adj_to;
  $adj_all[$v] = $adj_v;
  $g['adj'] = $adj_all;
  return $pushed;
};
}
  $i = $i + 1;
  $ptr[$v] = $i;
};
  $g['ptr'] = $ptr;
  $adj_all[$v] = $adj_v;
  $g['adj'] = $adj_all;
  return 0;
}
function max_flow(&$g, $source, $sink) {
  global $INF, $graph;
  $flow = 0;
  $l = 0;
  while ($l < 31) {
  $threshold = pow2(30 - $l);
  while (true) {
  $lvl = [];
  $ptr = [];
  $i = 0;
  while ($i < $g['n']) {
  $lvl = _append($lvl, 0);
  $ptr = _append($ptr, 0);
  $i = $i + 1;
};
  $g['lvl'] = $lvl;
  $g['ptr'] = $ptr;
  $qi = 0;
  $qe = 1;
  $lvl[$source] = 1;
  $g['lvl'] = $lvl;
  $q = $g['q'];
  $q[0] = $source;
  while ($qi < $qe && $g['lvl'][$sink] == 0) {
  $v = $q[$qi];
  $qi = $qi + 1;
  $edges = $g['adj'][$v];
  $j = 0;
  while ($j < count($edges)) {
  $e = $edges[$j];
  $to = $e[0];
  $residual = $e[2] - $e[3];
  $lvl_inner = $g['lvl'];
  if ($lvl_inner[$to] == 0 && $residual >= $threshold) {
  $q[$qe] = $to;
  $qe = $qe + 1;
  $lvl_inner[$to] = $lvl_inner[$v] + 1;
  $g['lvl'] = $lvl_inner;
}
  $j = $j + 1;
};
};
  $p = dfs($g, $source, $sink, $INF);
  while ($p > 0) {
  $flow = $flow + $p;
  $p = dfs($g, $source, $sink, $INF);
};
  if ($g['lvl'][$sink] == 0) {
  break;
}
};
  $l = $l + 1;
};
  return $flow;
}
$graph = new_dinic(10);
$source = 0;
$sink = 9;
$v = 1;
while ($v < 5) {
  add_edge($graph, $source, $v, 1, 0);
  $v = $v + 1;
}
$v = 5;
while ($v < 9) {
  add_edge($graph, $v, $sink, 1, 0);
  $v = $v + 1;
}
$v = 1;
while ($v < 5) {
  add_edge($graph, $v, $v + 4, 1, 0);
  $v = $v + 1;
}
echo rtrim(_str(max_flow($graph, $source, $sink))), PHP_EOL;
