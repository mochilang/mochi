<?php
ini_set('memory_limit', '-1');
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
function empty_stack() {
  return ['nodes' => [], 'head' => 0 - 1];
}
function push($stack, $value) {
  $nodes = $stack['nodes'];
  $idx = count($nodes);
  $new_node = ['data' => $value, 'next' => $stack['head'], 'prev' => 0 - 1];
  $nodes = _append($nodes, $new_node);
  if ($stack['head'] != 0 - 1) {
  $head_node = $nodes[$stack['head']];
  $head_node['prev'] = $idx;
  $nodes[$stack['head']] = $head_node;
}
  return ['nodes' => $nodes, 'head' => $idx];
}
function pop($stack) {
  if ($stack['head'] == 0 - 1) {
  return ['stack' => $stack, 'value' => 0, 'ok' => false];
}
  $nodes = $stack['nodes'];
  $head_node = $nodes[$stack['head']];
  $value = $head_node['data'];
  $next_idx = $head_node['next'];
  if ($next_idx != 0 - 1) {
  $next_node = $nodes[$next_idx];
  $next_node['prev'] = 0 - 1;
  $nodes[$next_idx] = $next_node;
}
  $new_stack = ['nodes' => $nodes, 'head' => $next_idx];
  return ['stack' => $new_stack, 'value' => $value, 'ok' => true];
}
function top($stack) {
  if ($stack['head'] == 0 - 1) {
  return ['value' => 0, 'ok' => false];
}
  $node = $stack['nodes'][$stack['head']];
  return ['value' => $node['data'], 'ok' => true];
}
function size($stack) {
  $count = 0;
  $idx = $stack['head'];
  while ($idx != 0 - 1) {
  $count = $count + 1;
  $node = $stack['nodes'][$idx];
  $idx = $node['next'];
};
  return $count;
}
function is_empty($stack) {
  return $stack['head'] == 0 - 1;
}
function print_stack($stack) {
  echo rtrim('stack elements are:'), PHP_EOL;
  $idx = $stack['head'];
  $s = '';
  while ($idx != 0 - 1) {
  $node = $stack['nodes'][$idx];
  $s = $s . _str($node['data']) . '->';
  $idx = $node['next'];
};
  if (strlen($s) > 0) {
  echo rtrim($s), PHP_EOL;
}
}
function main() {
  $stack = empty_stack();
  echo rtrim('Stack operations using Doubly LinkedList'), PHP_EOL;
  $stack = push($stack, 4);
  $stack = push($stack, 5);
  $stack = push($stack, 6);
  $stack = push($stack, 7);
  print_stack($stack);
  $t = top($stack);
  if ($t['ok']) {
  echo rtrim('Top element is ' . _str($t['value'])), PHP_EOL;
} else {
  echo rtrim('Top element is None'), PHP_EOL;
}
  echo rtrim('Size of the stack is ' . _str(size($stack))), PHP_EOL;
  $p = pop($stack);
  $stack = $p['stack'];
  $p = pop($stack);
  $stack = $p['stack'];
  print_stack($stack);
  echo rtrim('stack is empty: ' . _str(is_empty($stack))), PHP_EOL;
}
main();
