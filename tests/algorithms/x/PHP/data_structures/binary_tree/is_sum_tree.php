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
$__start_mem = memory_get_usage();
$__start = _now();
  function tree_sum($nodes, $idx) {
  if ($idx == (-1)) {
  return 0;
}
  $node = $nodes[$idx];
  return $node['value'] + tree_sum($nodes, $node['left']) + tree_sum($nodes, $node['right']);
};
  function is_sum_node($nodes, $idx) {
  $node = $nodes[$idx];
  if ($node['left'] == (-1) && $node['right'] == (-1)) {
  return true;
}
  $left_sum = tree_sum($nodes, $node['left']);
  $right_sum = tree_sum($nodes, $node['right']);
  if ($node['value'] != $left_sum + $right_sum) {
  return false;
}
  $left_ok = true;
  if ($node['left'] != (-1)) {
  $left_ok = is_sum_node($nodes, $node['left']);
}
  $right_ok = true;
  if ($node['right'] != (-1)) {
  $right_ok = is_sum_node($nodes, $node['right']);
}
  return $left_ok && $right_ok;
};
  function build_a_tree() {
  return [['left' => 1, 'right' => 2, 'value' => 11], ['left' => 3, 'right' => 4, 'value' => 2], ['left' => 5, 'right' => 6, 'value' => 29], ['left' => (-1), 'right' => (-1), 'value' => 1], ['left' => (-1), 'right' => (-1), 'value' => 7], ['left' => (-1), 'right' => (-1), 'value' => 15], ['left' => 7, 'right' => (-1), 'value' => 40], ['left' => (-1), 'right' => (-1), 'value' => 35]];
};
  function build_a_sum_tree() {
  return [['left' => 1, 'right' => 2, 'value' => 26], ['left' => 3, 'right' => 4, 'value' => 10], ['left' => (-1), 'right' => 5, 'value' => 3], ['left' => (-1), 'right' => (-1), 'value' => 4], ['left' => (-1), 'right' => (-1), 'value' => 6], ['left' => (-1), 'right' => (-1), 'value' => 3]];
};
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
