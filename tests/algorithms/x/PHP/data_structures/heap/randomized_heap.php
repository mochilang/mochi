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
$__start_mem = memory_get_usage();
$__start = _now();
  $NIL = 0 - 1;
  $seed = 1;
  function set_seed($s) {
  global $NIL, $nodes, $root, $seed;
  $seed = $s;
};
  function randint($a, $b) {
  global $NIL, $nodes, $root, $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return ($seed % ($b - $a + 1)) + $a;
};
  function rand_bool() {
  global $NIL, $nodes, $root, $seed;
  return randint(0, 1) == 1;
};
  $nodes = [];
  $root = $NIL;
  function new_heap() {
  global $NIL, $nodes, $root, $seed;
  $nodes = [];
  $root = $NIL;
};
  function merge($r1, $r2) {
  global $NIL, $nodes, $root, $seed;
  if ($r1 == $NIL) {
  return $r2;
}
  if ($r2 == $NIL) {
  return $r1;
}
  if ($nodes[$r1]['value'] > $nodes[$r2]['value']) {
  $tmp = $r1;
  $r1 = $r2;
  $r2 = $tmp;
}
  if (rand_bool()) {
  $tmp = $nodes[$r1]['left'];
  $nodes[$r1]['left'] = $nodes[$r1]['right'];
  $nodes[$r1]['right'] = $tmp;
}
  $nodes[$r1]['left'] = merge($nodes[$r1]['left'], $r2);
  return $r1;
};
  function insert($value) {
  global $NIL, $nodes, $root, $seed;
  $node = ['left' => $NIL, 'right' => $NIL, 'value' => $value];
  $nodes = _append($nodes, $node);
  $idx = count($nodes) - 1;
  $root = merge($root, $idx);
};
  function top() {
  global $NIL, $nodes, $root, $seed;
  if ($root == $NIL) {
  return 0;
}
  return $nodes[$root]['value'];
};
  function pop() {
  global $NIL, $nodes, $root, $seed;
  $result = top();
  $l = $nodes[$root]['left'];
  $r = $nodes[$root]['right'];
  $root = merge($l, $r);
  return $result;
};
  function is_empty() {
  global $NIL, $nodes, $root, $seed;
  return $root == $NIL;
};
  function to_sorted_list() {
  global $NIL, $nodes, $root, $seed;
  $res = [];
  while (!is_empty()) {
  $res = _append($res, pop());
};
  return $res;
};
  set_seed(1);
  new_heap();
  insert(2);
  insert(3);
  insert(1);
  insert(5);
  insert(1);
  insert(7);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(to_sorted_list(), 1344)))))), PHP_EOL;
  new_heap();
  insert(1);
  insert(-1);
  insert(0);
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(to_sorted_list(), 1344)))))), PHP_EOL;
  new_heap();
  insert(3);
  insert(1);
  insert(3);
  insert(7);
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
