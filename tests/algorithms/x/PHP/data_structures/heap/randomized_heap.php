<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$NIL = 0 - 1;
$seed = 1;
function set_seed($s) {
  global $NIL, $seed, $nodes, $root;
  $seed = $s;
}
function randint($a, $b) {
  global $NIL, $seed, $nodes, $root;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return ($seed % ($b - $a + 1)) + $a;
}
function rand_bool() {
  global $NIL, $seed, $nodes, $root;
  return randint(0, 1) == 1;
}
$nodes = [];
$root = $NIL;
function new_heap() {
  global $NIL, $seed, $nodes, $root;
  $nodes = [];
  $root = $NIL;
}
function merge($r1, $r2) {
  global $NIL, $seed, $nodes, $root;
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
}
function insert($value) {
  global $NIL, $seed, $nodes, $root;
  $node = ['value' => $value, 'left' => $NIL, 'right' => $NIL];
  $nodes = _append($nodes, $node);
  $idx = count($nodes) - 1;
  $root = merge($root, $idx);
}
function top() {
  global $NIL, $seed, $nodes, $root;
  if ($root == $NIL) {
  return 0;
}
  return $nodes[$root]['value'];
}
function pop() {
  global $NIL, $seed, $nodes, $root;
  $result = top();
  $l = $nodes[$root]['left'];
  $r = $nodes[$root]['right'];
  $root = merge($l, $r);
  return $result;
}
function is_empty() {
  global $NIL, $seed, $nodes, $root;
  return $root == $NIL;
}
function to_sorted_list() {
  global $NIL, $seed, $nodes, $root;
  $res = [];
  while (!is_empty()) {
  $res = _append($res, pop());
};
  return $res;
}
set_seed(1);
new_heap();
insert(2);
insert(3);
insert(1);
insert(5);
insert(1);
insert(7);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(to_sorted_list(), 1344))))))), PHP_EOL;
new_heap();
insert(1);
insert(-1);
insert(0);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(to_sorted_list(), 1344))))))), PHP_EOL;
new_heap();
insert(3);
insert(1);
insert(3);
insert(7);
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
