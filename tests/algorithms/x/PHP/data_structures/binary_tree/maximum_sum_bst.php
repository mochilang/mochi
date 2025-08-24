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
  function min_int($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function max_int($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function solver($nodes, $idx) {
  if ($idx == 0 - 1) {
  return ['best' => 0, 'is_bst' => true, 'max_val' => -2147483648, 'min_val' => 2147483647, 'total' => 0];
}
  $node = $nodes[$idx];
  $left_info = solver($nodes, $node['left']);
  $right_info = solver($nodes, $node['right']);
  $current_best = max_int($left_info['best'], $right_info['best']);
  if ($left_info['is_bst'] && $right_info['is_bst'] && $left_info['max_val'] < $node['val'] && $node['val'] < $right_info['min_val']) {
  $sum_val = $left_info['total'] + $right_info['total'] + $node['val'];
  $current_best = max_int($current_best, $sum_val);
  return ['best' => $current_best, 'is_bst' => true, 'max_val' => max_int($right_info['max_val'], $node['val']), 'min_val' => min_int($left_info['min_val'], $node['val']), 'total' => $sum_val];
}
  return ['best' => $current_best, 'is_bst' => false, 'max_val' => 0, 'min_val' => 0, 'total' => 0];
};
  function max_sum_bst($nodes, $root) {
  $info = solver($nodes, $root);
  return $info['best'];
};
  function main() {
  $t1_nodes = [['left' => 1, 'right' => 0 - 1, 'val' => 4], ['left' => 2, 'right' => 3, 'val' => 3], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 1], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 2]];
  echo rtrim(json_encode(max_sum_bst($t1_nodes, 0), 1344)), PHP_EOL;
  $t2_nodes = [['left' => 1, 'right' => 2, 'val' => -4], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => -2], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => -5]];
  echo rtrim(json_encode(max_sum_bst($t2_nodes, 0), 1344)), PHP_EOL;
  $t3_nodes = [['left' => 1, 'right' => 2, 'val' => 1], ['left' => 3, 'right' => 4, 'val' => 4], ['left' => 5, 'right' => 6, 'val' => 3], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 2], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 4], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 2], ['left' => 7, 'right' => 8, 'val' => 5], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 4], ['left' => 0 - 1, 'right' => 0 - 1, 'val' => 6]];
  echo rtrim(json_encode(max_sum_bst($t3_nodes, 0), 1344)), PHP_EOL;
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
