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
function pow2($exp) {
  $res = 1;
  $i = 0;
  while ($i < $exp) {
  $res = $res * 2;
  $i = $i + 1;
};
  return $res;
}
function create_sparse($max_node, &$parent) {
  $j = 1;
  while (pow2($j) < $max_node) {
  $i = 1;
  while ($i <= $max_node) {
  $parent[$j][$i] = $parent[$j - 1][$parent[$j - 1][$i]];
  $i = $i + 1;
};
  $j = $j + 1;
};
  return $parent;
}
function lowest_common_ancestor($u, $v, $level, $parent) {
  if ($level[$u] < $level[$v]) {
  $temp = $u;
  $u = $v;
  $v = $temp;
}
  $i = 18;
  while ($i >= 0) {
  if ($level[$u] - pow2($i) >= $level[$v]) {
  $u = $parent[$i][$u];
}
  $i = $i - 1;
};
  if ($u == $v) {
  return $u;
}
  $i = 18;
  while ($i >= 0) {
  $pu = $parent[$i][$u];
  $pv = $parent[$i][$v];
  if ($pu != 0 && $pu != $pv) {
  $u = $pu;
  $v = $pv;
}
  $i = $i - 1;
};
  return $parent[0][$u];
}
function breadth_first_search(&$level, &$parent, $max_node, $graph, $root) {
  $level[$root] = 0;
  $q = [];
  $q = _append($q, $root);
  $head = 0;
  while ($head < count($q)) {
  $u = $q[$head];
  $head = $head + 1;
  $adj = $graph[$u];
  $j = 0;
  while ($j < count($adj)) {
  $v = $adj[$j];
  if ($level[$v] == 0 - 1) {
  $level[$v] = $level[$u] + 1;
  $parent[0][$v] = $u;
  $q = _append($q, $v);
}
  $j = $j + 1;
};
};
}
function main() {
  $max_node = 13;
  $parent = [];
  $i = 0;
  while ($i < 20) {
  $row = [];
  $j = 0;
  while ($j < $max_node + 10) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $parent = _append($parent, $row);
  $i = $i + 1;
};
  $level = [];
  $i = 0;
  while ($i < $max_node + 10) {
  $level = _append($level, 0 - 1);
  $i = $i + 1;
};
  $graph = [];
  $graph[1] = [2, 3, 4];
  $graph[2] = [5];
  $graph[3] = [6, 7];
  $graph[4] = [8];
  $graph[5] = [9, 10];
  $graph[6] = [11];
  $graph[7] = [];
  $graph[8] = [12, 13];
  $graph[9] = [];
  $graph[10] = [];
  $graph[11] = [];
  $graph[12] = [];
  $graph[13] = [];
  breadth_first_search($level, $parent, $max_node, $graph, 1);
  $parent = create_sparse($max_node, $parent);
  echo rtrim('LCA of node 1 and 3 is: ' . _str(lowest_common_ancestor(1, 3, $level, $parent))), PHP_EOL;
  echo rtrim('LCA of node 5 and 6 is: ' . _str(lowest_common_ancestor(5, 6, $level, $parent))), PHP_EOL;
  echo rtrim('LCA of node 7 and 11 is: ' . _str(lowest_common_ancestor(7, 11, $level, $parent))), PHP_EOL;
  echo rtrim('LCA of node 6 and 7 is: ' . _str(lowest_common_ancestor(6, 7, $level, $parent))), PHP_EOL;
  echo rtrim('LCA of node 4 and 12 is: ' . _str(lowest_common_ancestor(4, 12, $level, $parent))), PHP_EOL;
  echo rtrim('LCA of node 8 and 8 is: ' . _str(lowest_common_ancestor(8, 8, $level, $parent))), PHP_EOL;
}
main();
