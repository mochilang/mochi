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
function new_graph() {
  return ['edges' => [], 'num_nodes' => 0];
}
function add_edge($g, $u, $v, $w) {
  $es = $g['edges'];
  $es = _append($es, [$u => $u, $v => $v, $w => $w]);
  $n = $g['num_nodes'];
  if ($u > $n) {
  $n = $u;
}
  if ($v > $n) {
  $n = $v;
}
  return ['edges' => $es, 'num_nodes' => $n];
}
function make_ds($n) {
  $parent = [];
  $rank = [];
  $i = 0;
  while ($i <= $n) {
  $parent = _append($parent, $i);
  $rank = _append($rank, 0);
  $i = $i + 1;
};
  return ['parent' => $parent, 'rank' => $rank];
}
function find_set($ds, $x) {
  if ($ds['parent'][$x] == $x) {
  return ['ds' => $ds, 'root' => $x];
}
  $res = find_set($ds, $ds['parent'][$x]);
  $p = $res['ds']['parent'];
  $p[$x] = $res['root'];
  return ['ds' => ['parent' => $p, 'rank' => $res['ds']['rank']], 'root' => $res['root']];
}
function union_set($ds, $x, $y) {
  $fx = find_set($ds, $x);
  $ds1 = $fx['ds'];
  $x_root = $fx['root'];
  $fy = find_set($ds1, $y);
  $ds2 = $fy['ds'];
  $y_root = $fy['root'];
  if ($x_root == $y_root) {
  return $ds2;
}
  $p = $ds2['parent'];
  $r = $ds2['rank'];
  if ($r[$x_root] > $r[$y_root]) {
  $p[$y_root] = $x_root;
} else {
  $p[$x_root] = $y_root;
  if ($r[$x_root] == $r[$y_root]) {
  $r[$y_root] = $r[$y_root] + 1;
};
}
  return ['parent' => $p, 'rank' => $r];
}
function sort_edges($edges) {
  $arr = $edges;
  $i = 1;
  while ($i < count($arr)) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0) {
  $temp = $arr[$j];
  if ($temp['w'] > $key['w'] || ($temp['w'] == $key['w'] && ($temp['u'] > $key['u'] || ($temp['u'] == $key['u'] && $temp['v'] > $key['v'])))) {
  $arr[$j + 1] = $temp;
  $j = $j - 1;
} else {
  break;
}
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
}
function kruskal($g) {
  $edges = sort_edges($g['edges']);
  $ds = make_ds($g['num_nodes']);
  $mst_edges = [];
  $i = 0;
  $added = 0;
  while ($added < $g['num_nodes'] - 1 && $i < count($edges)) {
  $e = $edges[$i];
  $i = $i + 1;
  $fu = find_set($ds, $e['u']);
  $ds = $fu['ds'];
  $ru = $fu['root'];
  $fv = find_set($ds, $e['v']);
  $ds = $fv['ds'];
  $rv = $fv['root'];
  if ($ru != $rv) {
  $mst_edges = _append($mst_edges, $e);
  $added = $added + 1;
  $ds = union_set($ds, $ru, $rv);
}
};
  return ['edges' => $mst_edges, 'num_nodes' => $g['num_nodes']];
}
function print_mst($g) {
  $es = sort_edges($g['edges']);
  foreach ($es as $e) {
  echo rtrim(_str($e['u']) . '-' . _str($e['v']) . ':' . _str($e['w'])), PHP_EOL;
};
}
function main() {
  $g = new_graph();
  $g = add_edge($g, 1, 2, 1);
  $g = add_edge($g, 2, 3, 2);
  $g = add_edge($g, 3, 4, 1);
  $g = add_edge($g, 3, 5, 100);
  $g = add_edge($g, 4, 5, 5);
  $mst = kruskal($g);
  print_mst($mst);
}
main();
