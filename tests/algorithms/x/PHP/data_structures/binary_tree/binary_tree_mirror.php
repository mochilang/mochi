<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function binary_tree_mirror_dict(&$tree, $root) {
  if (($root == 0) || (!(array_key_exists($root, $tree)))) {
  return;
}
  $children = $tree[$root];
  $left = $children[0];
  $right = $children[1];
  $tree[$root] = [$right, $left];
  binary_tree_mirror_dict($tree, $left);
  binary_tree_mirror_dict($tree, $right);
};
  function binary_tree_mirror($binary_tree, $root) {
  if (count($binary_tree) == 0) {
  _panic('binary tree cannot be empty');
}
  if (!(array_key_exists($root, $binary_tree))) {
  _panic('root ' . _str($root) . ' is not present in the binary_tree');
}
  $tree_copy = [];
  foreach (array_keys($binary_tree) as $k) {
  $tree_copy[$k] = $binary_tree[$k];
};
  binary_tree_mirror_dict($tree_copy, $root);
  return $tree_copy;
};
  function main() {
  $binary_tree = [1 => [2, 3], 2 => [4, 5], 3 => [6, 7], 7 => [8, 9]];
  echo rtrim('Binary tree: ' . _str($binary_tree)), PHP_EOL;
  $mirrored = binary_tree_mirror($binary_tree, 1);
  echo rtrim('Binary tree mirror: ' . _str($mirrored)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
