<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function new_node($prefix, $is_leaf) {
  return ['prefix' => $prefix, 'is_leaf' => $is_leaf, 'children' => []];
}
function new_tree() {
  $nodes = [new_node('', false)];
  return ['nodes' => $nodes];
}
function match_prefix($node, $word) {
  $x = 0;
  $p = $node['prefix'];
  $w = $word;
  $min_len = strlen($p);
  if (strlen($w) < $min_len) {
  $min_len = strlen($w);
}
  while ($x < $min_len) {
  if (substr($p, $x, $x + 1 - $x) != substr($w, $x, $x + 1 - $x)) {
  break;
}
  $x = $x + 1;
};
  $common = substr($p, 0, $x - 0);
  $rem_prefix = substr($p, $x, strlen($p) - $x);
  $rem_word = substr($w, $x, strlen($w) - $x);
  return ['common' => $common, 'rem_prefix' => $rem_prefix, 'rem_word' => $rem_word];
}
function insert_many($tree, $words) {
  foreach ($words as $w) {
  insert($tree, 0, $w);
};
}
function insert(&$tree, $idx, $word) {
  $nodes = $tree['nodes'];
  $node = $nodes[$idx];
  if (($node['prefix'] == $word) && (!$node['is_leaf'])) {
  $node['is_leaf'] = true;
  $nodes[$idx] = $node;
  $tree['nodes'] = $nodes;
  return;
}
  $first = substr($word, 0, 1 - 0);
  $children = $node['children'];
  if (!has_key($children, $first)) {
  $new_idx = count($nodes);
  $nodes = _append($nodes, new_node($word, true));
  $children[$first] = $new_idx;
  $node['children'] = $children;
  $nodes[$idx] = $node;
  $tree['nodes'] = $nodes;
  return;
}
  $child_idx = $children[$first];
  $child = $nodes[$child_idx];
  $res = match_prefix($child, $word);
  if ($res['rem_prefix'] == '') {
  insert($tree, $child_idx, $res['rem_word']);
  return;
}
  $child['prefix'] = $res['rem_prefix'];
  $nodes[$child_idx] = $child;
  $new_children = [];
  $new_children[substr($res['rem_prefix'], 0, 1 - 0)] = $child_idx;
  $new_idx = count($nodes);
  $nodes = _append($nodes, new_node($res['common'], false));
  $nodes[$new_idx]['children'] = $new_children;
  if ($res['rem_word'] == '') {
  $nodes[$new_idx]['is_leaf'] = true;
} else {
  insert($tree, $new_idx, $res['rem_word']);
}
  $children[$first] = $new_idx;
  $node['children'] = $children;
  $nodes[$idx] = $node;
  $tree['nodes'] = $nodes;
}
function find($tree, $idx, $word) {
  $nodes = $tree['nodes'];
  $node = $nodes[$idx];
  $first = substr($word, 0, 1 - 0);
  $children = $node['children'];
  if (!has_key($children, $first)) {
  return false;
}
  $child_idx = $children[$first];
  $child = $nodes[$child_idx];
  $res = match_prefix($child, $word);
  if ($res['rem_prefix'] != '') {
  return false;
}
  if ($res['rem_word'] == '') {
  return $child['is_leaf'];
}
  return find($tree, $child_idx, $res['rem_word']);
}
function remove_key($m, $k) {
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
}
function has_key($m, $k) {
  foreach (array_keys($m) as $key) {
  if ($key == $k) {
  return true;
}
};
  return false;
}
function delete(&$tree, $idx, $word) {
  $nodes = $tree['nodes'];
  $node = $nodes[$idx];
  $first = substr($word, 0, 1 - 0);
  $children = $node['children'];
  if (!has_key($children, $first)) {
  return false;
}
  $child_idx = $children[$first];
  $child = $nodes[$child_idx];
  $res = match_prefix($child, $word);
  if ($res['rem_prefix'] != '') {
  return false;
}
  if ($res['rem_word'] != '') {
  $deleted = delete($tree, $child_idx, $res['rem_word']);
  if ($deleted) {
  $nodes = $tree['nodes'];
  $node = $nodes[$idx];
};
  return $deleted;
}
  if (!$child['is_leaf']) {
  return false;
}
  if (_len($child['children']) == 0) {
  $children = remove_key($children, $first);
  $node['children'] = $children;
  $nodes[$idx] = $node;
  $tree['nodes'] = $nodes;
  if ((count($children) == 1) && (!$node['is_leaf'])) {
  $only_key = '';
  foreach (array_keys($children) as $k) {
  $only_key = $k;
};
  $merge_idx = $children[$only_key];
  $merge_node = $nodes[$merge_idx];
  $node['is_leaf'] = $merge_node['is_leaf'];
  $node['prefix'] = $node['prefix'] . $merge_node['prefix'];
  $node['children'] = $merge_node['children'];
  $nodes[$idx] = $node;
  $tree['nodes'] = $nodes;
};
} else {
  if (_len($child['children']) > 1) {
  $child['is_leaf'] = false;
  $nodes[$child_idx] = $child;
  $tree['nodes'] = $nodes;
} else {
  $only_key = '';
  foreach (array_keys($child['children']) as $k) {
  $only_key = $k;
};
  $merge_idx = $child['children'][$only_key];
  $merge_node = $nodes[$merge_idx];
  $child['is_leaf'] = $merge_node['is_leaf'];
  $child['prefix'] = $child['prefix'] . $merge_node['prefix'];
  $child['children'] = $merge_node['children'];
  $nodes[$child_idx] = $child;
  $tree['nodes'] = $nodes;
};
}
  return true;
}
function print_tree($tree, $idx, $height) {
  $nodes = $tree['nodes'];
  $node = $nodes[$idx];
  if ($node['prefix'] != '') {
  $line = '';
  $i = 0;
  while ($i < $height) {
  $line = $line . '-';
  $i = $i + 1;
};
  $line = $line . ' ' . $node['prefix'];
  if ($node['is_leaf']) {
  $line = $line . '  (leaf)';
};
  echo rtrim($line), PHP_EOL;
}
  $children = $node['children'];
  foreach (array_keys($children) as $k) {
  $child_idx = $children[$k];
  print_tree($tree, $child_idx, $height + 1);
};
}
function test_trie() {
  $words = ['banana', 'bananas', 'bandana', 'band', 'apple', 'all', 'beast'];
  $tree = new_tree();
  insert_many($tree, $words);
  $ok = true;
  foreach ($words as $w) {
  if (!find($tree, 0, $w)) {
  $ok = false;
}
};
  if (find($tree, 0, 'bandanas')) {
  $ok = false;
}
  if (find($tree, 0, 'apps')) {
  $ok = false;
}
  delete($tree, 0, 'all');
  if (find($tree, 0, 'all')) {
  $ok = false;
}
  delete($tree, 0, 'banana');
  if (find($tree, 0, 'banana')) {
  $ok = false;
}
  if (!find($tree, 0, 'bananas')) {
  $ok = false;
}
  return $ok;
}
function pytests() {
  if (!test_trie()) {
  $panic('test failed');
}
}
function main() {
  $tree = new_tree();
  $words = ['banana', 'bananas', 'bandanas', 'bandana', 'band', 'apple', 'all', 'beast'];
  insert_many($tree, $words);
  echo rtrim('Words: ' . _str($words)), PHP_EOL;
  echo rtrim('Tree:'), PHP_EOL;
  print_tree($tree, 0, 0);
}
main();
