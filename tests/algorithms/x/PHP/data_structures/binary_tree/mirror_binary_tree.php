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
$__start_mem = memory_get_usage();
$__start = _now();
  function mirror_node(&$left, &$right, $idx) {
  if ($idx == (-1)) {
  return;
}
  $temp = $left[$idx];
  $left[$idx] = $right[$idx];
  $right[$idx] = $temp;
  mirror_node($left, $right, $left[$idx]);
  mirror_node($left, $right, $right[$idx]);
};
  function mirror($tree) {
  mirror_node($tree['left'], $tree['right'], $tree['root']);
  return $tree;
};
  function inorder($tree, $idx) {
  if ($idx == (-1)) {
  return [];
}
  $left_vals = inorder($tree, $tree['left'][$idx]);
  $right_vals = inorder($tree, $tree['right'][$idx]);
  return array_merge(array_merge($left_vals, [$tree['values'][$idx]]), $right_vals);
};
  function make_tree_zero() {
  return ['left' => [-1], 'right' => [-1], 'root' => 0, 'values' => [0]];
};
  function make_tree_seven() {
  return ['left' => [1, 3, 5, -1, -1, -1, -1], 'right' => [2, 4, 6, -1, -1, -1, -1], 'root' => 0, 'values' => [1, 2, 3, 4, 5, 6, 7]];
};
  function make_tree_nine() {
  return ['left' => [1, 3, -1, 6, -1, -1, -1, -1, -1], 'right' => [2, 4, 5, 7, 8, -1, -1, -1, -1], 'root' => 0, 'values' => [1, 2, 3, 4, 5, 6, 7, 8, 9]];
};
  function main() {
  $names = ['zero', 'seven', 'nine'];
  $trees = [make_tree_zero(), make_tree_seven(), make_tree_nine()];
  $i = 0;
  while ($i < count($trees)) {
  $tree = $trees[$i];
  echo rtrim('      The ' . $names[$i] . ' tree: ' . _str(inorder($tree, $tree['root']))), PHP_EOL;
  $mirrored = mirror($tree);
  echo rtrim('Mirror of ' . $names[$i] . ' tree: ' . _str(inorder($mirrored, $mirrored['root']))), PHP_EOL;
  $i = $i + 1;
};
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
