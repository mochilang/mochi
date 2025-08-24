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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $node_data = [0];
  $left_child = [0];
  $right_child = [0];
  function new_node($value) {
  global $left_child, $node_data, $right_child, $root, $vals;
  $node_data = _append($node_data, $value);
  $left_child = _append($left_child, 0);
  $right_child = _append($right_child, 0);
  return count($node_data) - 1;
};
  function build_tree() {
  global $left_child, $node_data, $right_child, $vals;
  $root = new_node(1);
  $n2 = new_node(2);
  $n5 = new_node(5);
  $n3 = new_node(3);
  $n4 = new_node(4);
  $n6 = new_node(6);
  $left_child[$root] = $n2;
  $right_child[$root] = $n5;
  $left_child[$n2] = $n3;
  $right_child[$n2] = $n4;
  $right_child[$n5] = $n6;
  return $root;
};
  function flatten($root) {
  global $left_child, $node_data, $right_child, $vals;
  if ($root == 0) {
  return [];
}
  $res = [$node_data[$root]];
  $left_vals = flatten($left_child[$root]);
  $right_vals = flatten($right_child[$root]);
  $i = 0;
  while ($i < count($left_vals)) {
  $res = _append($res, $left_vals[$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($right_vals)) {
  $res = _append($res, $right_vals[$i]);
  $i = $i + 1;
};
  return $res;
};
  function display($values) {
  global $left_child, $node_data, $right_child, $root, $vals;
  $s = '';
  $i = 0;
  while ($i < count($values)) {
  if ($i == 0) {
  $s = _str($values[$i]);
} else {
  $s = $s . ' ' . _str($values[$i]);
}
  $i = $i + 1;
};
  echo rtrim($s), PHP_EOL;
};
  echo rtrim('Flattened Linked List:'), PHP_EOL;
  $root = build_tree();
  $vals = flatten($root);
  display($vals);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
