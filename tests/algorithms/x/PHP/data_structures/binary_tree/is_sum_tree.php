<?php
ini_set('memory_limit', '-1');
function tree_sum($nodes, $idx) {
  if ($idx == (-1)) {
  return 0;
}
  $node = $nodes[$idx];
  return $node['value'] + tree_sum($nodes, $node['left']) + tree_sum($nodes, $node['right']);
}
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
}
function build_a_tree() {
  return [['value' => 11, 'left' => 1, 'right' => 2], ['value' => 2, 'left' => 3, 'right' => 4], ['value' => 29, 'left' => 5, 'right' => 6], ['value' => 1, 'left' => (-1), 'right' => (-1)], ['value' => 7, 'left' => (-1), 'right' => (-1)], ['value' => 15, 'left' => (-1), 'right' => (-1)], ['value' => 40, 'left' => 7, 'right' => (-1)], ['value' => 35, 'left' => (-1), 'right' => (-1)]];
}
function build_a_sum_tree() {
  return [['value' => 26, 'left' => 1, 'right' => 2], ['value' => 10, 'left' => 3, 'right' => 4], ['value' => 3, 'left' => (-1), 'right' => 5], ['value' => 4, 'left' => (-1), 'right' => (-1)], ['value' => 6, 'left' => (-1), 'right' => (-1)], ['value' => 3, 'left' => (-1), 'right' => (-1)]];
}
