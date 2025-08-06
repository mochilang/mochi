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
  function make_symmetric_tree() {
  global $symmetric_tree, $asymmetric_tree;
  return [[1, 1, 2], [2, 3, 4], [2, 5, 6], [3, -1, -1], [4, -1, -1], [4, -1, -1], [3, -1, -1]];
};
  function make_asymmetric_tree() {
  global $symmetric_tree, $asymmetric_tree;
  return [[1, 1, 2], [2, 3, 4], [2, 5, 6], [3, -1, -1], [4, -1, -1], [3, -1, -1], [4, -1, -1]];
};
  function is_symmetric_tree($tree) {
  global $symmetric_tree, $asymmetric_tree;
  $stack = [$tree[0][1], $tree[0][2]];
  while (count($stack) >= 2) {
  $left = $stack[count($stack) - 2];
  $right = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 2 - 0);
  if ($left == (-1) && $right == (-1)) {
  continue;
}
  if ($left == (-1) || $right == (-1)) {
  return false;
}
  $lnode = $tree[$left];
  $rnode = $tree[$right];
  if ($lnode[0] != $rnode[0]) {
  return false;
}
  $stack = _append($stack, $lnode[1]);
  $stack = _append($stack, $rnode[2]);
  $stack = _append($stack, $lnode[2]);
  $stack = _append($stack, $rnode[1]);
};
  return true;
};
  $symmetric_tree = make_symmetric_tree();
  $asymmetric_tree = make_asymmetric_tree();
  echo rtrim(_str(is_symmetric_tree($symmetric_tree))), PHP_EOL;
  echo rtrim(_str(is_symmetric_tree($asymmetric_tree))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
