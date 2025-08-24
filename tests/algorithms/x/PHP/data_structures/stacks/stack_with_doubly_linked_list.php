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
  function empty_stack() {
  return ['head' => 0 - 1, 'nodes' => []];
};
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
  return ['head' => $idx, 'nodes' => $nodes];
};
  function pop($stack) {
  if ($stack['head'] == 0 - 1) {
  return ['ok' => false, 'stack' => $stack, 'value' => 0];
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
  $new_stack = ['head' => $next_idx, 'nodes' => $nodes];
  return ['ok' => true, 'stack' => $new_stack, 'value' => $value];
};
  function top($stack) {
  if ($stack['head'] == 0 - 1) {
  return ['ok' => false, 'value' => 0];
}
  $node = $stack['nodes'][$stack['head']];
  return ['ok' => true, 'value' => $node['data']];
};
  function size($stack) {
  $count = 0;
  $idx = $stack['head'];
  while ($idx != 0 - 1) {
  $count = $count + 1;
  $node = $stack['nodes'][$idx];
  $idx = $node['next'];
};
  return $count;
};
  function is_empty($stack) {
  return $stack['head'] == 0 - 1;
};
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
};
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
