<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function new_trie() {
  global $trie;
  return ['nodes' => [['children' => [], 'is_leaf' => false]]];
}
function remove_key($m, $k) {
  global $trie;
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
}
function insert(&$trie, $word) {
  $nodes = $trie['nodes'];
  $curr = 0;
  $i = 0;
  while ($i < strlen($word)) {
  $ch = substr($word, $i, $i + 1 - $i);
  $child_idx = -1;
  $children = $nodes[$curr]['children'];
  if (isset($children[$ch])) {
  $child_idx = $children[$ch];
} else {
  $new_node = ['children' => [], 'is_leaf' => false];
  $nodes = _append($nodes, $new_node);
  $child_idx = count($nodes) - 1;
  $new_children = $children;
  $new_children[$ch] = $child_idx;
  $node = $nodes[$curr];
  $node['children'] = $new_children;
  $nodes[$curr] = $node;
}
  $curr = $child_idx;
  $i = $i + 1;
};
  $node = $nodes[$curr];
  $node['is_leaf'] = true;
  $nodes[$curr] = $node;
  $trie['nodes'] = $nodes;
}
function insert_many(&$trie, $words) {
  foreach ($words as $w) {
  insert($trie, $w);
};
}
function find($trie, $word) {
  $nodes = $trie['nodes'];
  $curr = 0;
  $i = 0;
  while ($i < strlen($word)) {
  $ch = substr($word, $i, $i + 1 - $i);
  $children = $nodes[$curr]['children'];
  if (!(isset($children[$ch]))) {
  return false;
}
  $curr = $children[$ch];
  $i = $i + 1;
};
  $node = $nodes[$curr];
  return $node['is_leaf'];
}
function delete(&$trie, $word) {
  $nodes = $trie['nodes'];
  $_delete = null;
$_delete = function($idx, $pos) use (&$_delete, $trie, $word, &$nodes) {
  if ($pos == strlen($word)) {
  $node = $nodes[$idx];
  if ($node['is_leaf'] == false) {
  return false;
};
  $node['is_leaf'] = false;
  $nodes[$idx] = $node;
  return _len($node['children']) == 0;
}
  $node = $nodes[$idx];
  $children = $node['children'];
  $ch = substr($word, $pos, $pos + 1 - $pos);
  if (!(array_key_exists($ch, $children))) {
  return false;
}
  $child_idx = $children[$ch];
  $should_delete = $_delete($child_idx, $pos + 1);
  $node = $nodes[$idx];
  if ($should_delete) {
  $new_children = remove_key($node['children'], $ch);
  $node['children'] = $new_children;
  $nodes[$idx] = $node;
  return count($new_children) == 0 && $node['is_leaf'] == false;
}
  $nodes[$idx] = $node;
  return false;
};
  $_delete(0, 0);
  $trie['nodes'] = $nodes;
}
function print_words($trie) {
  $dfs = null;
$dfs = function($idx, $word) use (&$dfs, $trie) {
  $node = $trie['nodes'][$idx];
  if ($node['is_leaf']) {
  echo rtrim($word), PHP_EOL;
}
  foreach (array_keys($node['children']) as $key) {
  $dfs($node['children'][$key], $word . $key);
};
};
  $dfs(0, '');
}
function test_trie() {
  $words = ['banana', 'bananas', 'bandana', 'band', 'apple', 'all', 'beast'];
  $trie = new_trie();
  insert_many($trie, $words);
  $ok = true;
  foreach ($words as $w) {
  $ok = $ok && find($trie, $w);
};
  $ok = $ok && find($trie, 'banana');
  $t = find($trie, 'bandanas');
  $ok = $ok && ($t == false);
  $t2 = find($trie, 'apps');
  $ok = $ok && ($t2 == false);
  $ok = $ok && find($trie, 'apple');
  $ok = $ok && find($trie, 'all');
  delete($trie, 'all');
  $t3 = find($trie, 'all');
  $ok = $ok && ($t3 == false);
  delete($trie, 'banana');
  $t4 = find($trie, 'banana');
  $ok = $ok && ($t4 == false);
  $ok = $ok && find($trie, 'bananas');
  return $ok;
}
function print_results($msg, $passes) {
  global $trie;
  if ($passes) {
  echo rtrim($msg . ' works!'), PHP_EOL;
} else {
  echo rtrim($msg . ' doesn\'t work :('), PHP_EOL;
}
}
$trie = new_trie();
print_results('Testing trie functionality', test_trie());
