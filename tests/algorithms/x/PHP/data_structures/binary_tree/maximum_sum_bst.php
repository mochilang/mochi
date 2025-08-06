<?php
ini_set('memory_limit', '-1');
function min_int($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
}
function max_int($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
}
function solver($nodes, $idx) {
  if ($idx == 0 - 1) {
  return ['is_bst' => true, 'min_val' => 2147483647, 'max_val' => -2147483648, 'total' => 0, 'best' => 0];
}
  $node = $nodes[$idx];
  $left_info = solver($nodes, $node['left']);
  $right_info = solver($nodes, $node['right']);
  $current_best = max_int($left_info['best'], $right_info['best']);
  if ($left_info['is_bst'] && $right_info['is_bst'] && $left_info['max_val'] < $node['val'] && $node['val'] < $right_info['min_val']) {
  $sum_val = $left_info['total'] + $right_info['total'] + $node['val'];
  $current_best = max_int($current_best, $sum_val);
  return ['is_bst' => true, 'min_val' => min_int($left_info['min_val'], $node['val']), 'max_val' => max_int($right_info['max_val'], $node['val']), 'total' => $sum_val, 'best' => $current_best];
}
  return ['is_bst' => false, 'min_val' => 0, 'max_val' => 0, 'total' => 0, 'best' => $current_best];
}
function max_sum_bst($nodes, $root) {
  $info = solver($nodes, $root);
  return $info['best'];
}
function main() {
  $t1_nodes = [['val' => 4, 'left' => 1, 'right' => 0 - 1], ['val' => 3, 'left' => 2, 'right' => 3], ['val' => 1, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => 2, 'left' => 0 - 1, 'right' => 0 - 1]];
  echo rtrim(json_encode(max_sum_bst($t1_nodes, 0), 1344)), PHP_EOL;
  $t2_nodes = [['val' => -4, 'left' => 1, 'right' => 2], ['val' => -2, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => -5, 'left' => 0 - 1, 'right' => 0 - 1]];
  echo rtrim(json_encode(max_sum_bst($t2_nodes, 0), 1344)), PHP_EOL;
  $t3_nodes = [['val' => 1, 'left' => 1, 'right' => 2], ['val' => 4, 'left' => 3, 'right' => 4], ['val' => 3, 'left' => 5, 'right' => 6], ['val' => 2, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => 4, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => 2, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => 5, 'left' => 7, 'right' => 8], ['val' => 4, 'left' => 0 - 1, 'right' => 0 - 1], ['val' => 6, 'left' => 0 - 1, 'right' => 0 - 1]];
  echo rtrim(json_encode(max_sum_bst($t3_nodes, 0), 1344)), PHP_EOL;
}
main();
