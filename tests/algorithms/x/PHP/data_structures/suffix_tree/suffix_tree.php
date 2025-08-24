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
  function new_node() {
  global $st;
  return ['children' => [], 'end' => -1, 'is_end_of_string' => false, 'start' => -1];
};
  function has_key($m, $k) {
  global $st;
  foreach (array_keys($m) as $key) {
  if ($key == $k) {
  return true;
}
};
  return false;
};
  function add_suffix($tree, $suffix, $index) {
  global $st;
  $nodes = $tree['nodes'];
  $node_idx = 0;
  $j = 0;
  while ($j < strlen($suffix)) {
  $ch = substr($suffix, $j, $j + 1 - $j);
  $node = $nodes[$node_idx];
  $children = $node['children'];
  if (!has_key($children, $ch)) {
  $nodes = _append($nodes, new_node());
  $new_idx = count($nodes) - 1;
  $children[$ch] = $new_idx;
}
  $node['children'] = $children;
  $nodes[$node_idx] = $node;
  $node_idx = $children[$ch];
  $j = $j + 1;
};
  $node = $nodes[$node_idx];
  $node['is_end_of_string'] = true;
  $node['start'] = $index;
  $node['end'] = $index + strlen($suffix) - 1;
  $nodes[$node_idx] = $node;
  $tree['nodes'] = $nodes;
  return $tree;
};
  function build_suffix_tree($tree) {
  global $st;
  $text = $tree['text'];
  $n = strlen($text);
  $i = 0;
  $t = $tree;
  while ($i < $n) {
  $suffix = '';
  $k = $i;
  while ($k < $n) {
  $suffix = $suffix . substr($text, $k, $k + 1 - $k);
  $k = $k + 1;
};
  $t = add_suffix($t, $suffix, $i);
  $i = $i + 1;
};
  return $t;
};
  function new_suffix_tree($text) {
  global $st;
  $tree = ['nodes' => [], 'text' => $text];
  $tree['nodes'] = _append($tree['nodes'], new_node());
  $tree = build_suffix_tree($tree);
  return $tree;
};
  function search($tree, $pattern) {
  global $st;
  $node_idx = 0;
  $i = 0;
  $nodes = $tree['nodes'];
  while ($i < strlen($pattern)) {
  $ch = substr($pattern, $i, $i + 1 - $i);
  $node = $nodes[$node_idx];
  $children = $node['children'];
  if (!has_key($children, $ch)) {
  return false;
}
  $node_idx = $children[$ch];
  $i = $i + 1;
};
  return true;
};
  $st = new_suffix_tree('bananas');
  echo rtrim(_str(search($st, 'ana'))), PHP_EOL;
  echo rtrim(_str(search($st, 'apple'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
