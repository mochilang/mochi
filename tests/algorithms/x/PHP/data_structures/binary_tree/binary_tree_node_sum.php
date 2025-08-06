<?php
ini_set('memory_limit', '-1');
function node_sum($tree, $index) {
  global $example;
  if ($index == (-1)) {
  return 0;
}
  $node = $tree[$index];
  return $node['value'] + node_sum($tree, $node['left']) + node_sum($tree, $node['right']);
}
$example = [['value' => 10, 'left' => 1, 'right' => 2], ['value' => 5, 'left' => 3, 'right' => -1], ['value' => -3, 'left' => 4, 'right' => 5], ['value' => 12, 'left' => -1, 'right' => -1], ['value' => 8, 'left' => -1, 'right' => -1], ['value' => 0, 'left' => -1, 'right' => -1]];
echo rtrim(json_encode(node_sum($example, 0), 1344)), PHP_EOL;
