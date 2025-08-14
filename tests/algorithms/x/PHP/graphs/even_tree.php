<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
$__start_mem = memory_get_usage();
$__start = _now();
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
};
  function even_tree() {
  global $tree;
  $visited = [];
  $res = dfs(1, $visited);
  return $res[1] - 1;
};
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
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
