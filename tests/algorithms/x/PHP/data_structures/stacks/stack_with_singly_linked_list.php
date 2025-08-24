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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function empty_stack() {
  return ['nodes' => [], 'top' => (-1)];
};
  function is_empty($stack) {
  return $stack['top'] == (-1);
};
  function push($stack, $item) {
  $new_node = ['next' => $stack['top'], 'value' => $item];
  $new_nodes = $stack['nodes'];
  $new_nodes = _append($new_nodes, $new_node);
  $new_top = count($new_nodes) - 1;
  return ['nodes' => $new_nodes, 'top' => $new_top];
};
  function pop($stack) {
  if ($stack['top'] == (-1)) {
  _panic('pop from empty stack');
}
  $node = ($stack['nodes'][$stack['top']]);
  $new_top = $node['next'];
  $new_stack = ['nodes' => $stack['nodes'], 'top' => $new_top];
  return ['stack' => $new_stack, 'value' => $node['value']];
};
  function peek($stack) {
  if ($stack['top'] == (-1)) {
  _panic('peek from empty stack');
}
  $node = ($stack['nodes'][$stack['top']]);
  return $node['value'];
};
  function clear($stack) {
  return ['nodes' => [], 'top' => (-1)];
};
  function main() {
  $stack = empty_stack();
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
  $stack = push($stack, '5');
  $stack = push($stack, '9');
  $stack = push($stack, 'python');
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim($res['value']), PHP_EOL;
  $stack = push($stack, 'algorithms');
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim($res['value']), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim($res['value']), PHP_EOL;
  $res = pop($stack);
  $stack = $res['stack'];
  echo rtrim($res['value']), PHP_EOL;
  echo rtrim(json_encode(is_empty($stack), 1344)), PHP_EOL;
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
