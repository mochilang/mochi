<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function empty_stack() {
  return ['nodes' => [], 'top' => (-1)];
}
function is_empty($stack) {
  return $stack['top'] == (-1);
}
function push($stack, $item) {
  $new_node = ['value' => $item, 'next' => $stack['top']];
  $new_nodes = $stack['nodes'];
  $new_nodes = _append($new_nodes, $new_node);
  $new_top = count($new_nodes) - 1;
  return ['nodes' => $new_nodes, 'top' => $new_top];
}
function pop($stack) {
  if ($stack['top'] == (-1)) {
  $panic('pop from empty stack');
}
  $node = ($stack['nodes'][$stack['top']]);
  $new_top = $node['next'];
  $new_stack = ['nodes' => $stack['nodes'], 'top' => $new_top];
  return ['stack' => $new_stack, 'value' => $node['value']];
}
function peek($stack) {
  if ($stack['top'] == (-1)) {
  $panic('peek from empty stack');
}
  $node = ($stack['nodes'][$stack['top']]);
  return $node['value'];
}
function clear($stack) {
  return ['nodes' => [], 'top' => (-1)];
}
function main() {
  $stack = empty_stack();
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
  $stack = push($stack, '5');
  $stack = push($stack, '9');
  $stack = push($stack, 'python');
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $stack = push($stack, 'algorithms');
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
}
main();
