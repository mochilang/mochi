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
$tree = [];
function dfs($start, &$visited) {
  global $tree;
  $size = 1;
  $cuts = 0;
  $visited[$start] = true;
  foreach ($tree[$start] as $v) {
  if (!(array_key_exists($v, $visited))) {
  $res = dfs($v, $visited);
  $size = $size + $res[0];
  $cuts = $cuts + $res[1];
}
};
  if ($size % 2 == 0) {
  $cuts = $cuts + 1;
}
  return [$size, $cuts];
}
function even_tree() {
  global $tree;
  $visited = [];
  $res = dfs(1, $visited);
  return $res[1] - 1;
}
function main() {
  global $tree;
  $edges = [[2, 1], [3, 1], [4, 3], [5, 2], [6, 1], [7, 2], [8, 6], [9, 8], [10, 8]];
  $i = 0;
  while ($i < count($edges)) {
  $u = $edges[$i][0];
  $v = $edges[$i][1];
  if (!(array_key_exists($u, $tree))) {
  $tree[$u] = [];
}
  if (!(array_key_exists($v, $tree))) {
  $tree[$v] = [];
}
  $tree[$u] = _append($tree[$u], $v);
  $tree[$v] = _append($tree[$v], $u);
  $i = $i + 1;
};
  echo rtrim(_str(even_tree())), PHP_EOL;
}
main();
