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
  function new_suffix_tree_node($children, $is_end_of_string, $start, $end, $suffix_link) {
  global $leaf, $leaf_check, $nodes, $root, $root_check;
  return ['children' => $children, 'end' => $end, 'is_end_of_string' => $is_end_of_string, 'start' => $start, 'suffix_link' => $suffix_link];
};
  function empty_suffix_tree_node() {
  global $leaf, $leaf_check, $nodes, $root, $root_check;
  return new_suffix_tree_node([], false, 0 - 1, 0 - 1, 0 - 1);
};
  function has_key($m, $k) {
  global $leaf, $leaf_check, $nodes, $root, $root_check;
  foreach (array_keys($m) as $key) {
  if ($key == $k) {
  return true;
}
};
  return false;
};
  $root = new_suffix_tree_node(['a' => 1], false, 0 - 1, 0 - 1, 0 - 1);
  $leaf = new_suffix_tree_node([], true, 0, 2, 0);
  $nodes = [$root, $leaf];
  $root_check = $nodes[0];
  $leaf_check = $nodes[1];
  echo rtrim(_str(has_key($root_check['children'], 'a'))), PHP_EOL;
  echo rtrim(_str($leaf_check['is_end_of_string'])), PHP_EOL;
  echo rtrim(_str($leaf_check['start'])), PHP_EOL;
  echo rtrim(_str($leaf_check['end'])), PHP_EOL;
  echo rtrim(_str($leaf_check['suffix_link'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
