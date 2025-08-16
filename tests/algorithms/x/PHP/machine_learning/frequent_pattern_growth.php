<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function make_node($name, $count, $parent) {
  return ['name' => $name, 'count' => $count, 'parent' => $parent, 'children' => [], 'node_link' => null];
}
function update_header($node_to_test, $target_node) {
  $current = $node_to_test;
  while ($current['node_link'] != null) {
  $current = $current['node_link'];
};
  $current['node_link'] = $target_node;
}
function update_tree($items, &$in_tree, &$header_table, $count) {
  $first = $items[0];
  $children = $in_tree['children'];
  if (isset($children[$first])) {
  $child = $children[$first];
  $child['count'] = $child['count'] + $count;
  $children[$first] = $child;
  $in_tree['children'] = $children;
} else {
  $new_node = make_node($first, $count, $in_tree);
  $children[$first] = $new_node;
  $in_tree['children'] = $children;
  $entry = $header_table[$first];
  if ($entry['node'] == null) {
  $entry['node'] = $new_node;
} else {
  update_header($entry['node'], $new_node);
};
  $header_table[$first] = $entry;
}
  if (count($items) > 1) {
  $rest = array_slice($items, 1, count($items) - 1);
  update_tree($rest, $children[$first], $header_table, $count);
}
}
function sort_items($items, $header_table) {
  $arr = $items;
  $i = 0;
  while ($i < count($arr)) {
  $j = $i + 1;
  while ($j < count($arr)) {
  if ($header_table[$arr[$i]]['count'] < $header_table[$arr[$j]]['count']) {
  $tmp = $arr[$i];
  $arr[$i] = $arr[$j];
  $arr[$j] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
}
function create_tree($data_set, $min_sup) {
  $counts = [];
  $i = 0;
  while ($i < count($data_set)) {
  $trans = $data_set[$i];
  $j = 0;
  while ($j < count($trans)) {
  $item = $trans[$j];
  if (array_key_exists($item, $counts)) {
  $counts[$item] = $counts[$item] + 1;
} else {
  $counts[$item] = 1;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $header_table = [];
  foreach (array_keys($counts) as $k) {
  $cnt = $counts[$k];
  if ($cnt >= $min_sup) {
  $header_table[$k] = ['count' => $cnt, 'node' => null];
}
};
  $freq_items = [];
  foreach (array_keys($header_table) as $k) {
  $freq_items = _append($freq_items, $k);
};
  if (count($freq_items) == 0) {
  return ['tree' => make_node('Null Set', 1, null), 'header' => []];
}
  $fp_tree = make_node('Null Set', 1, null);
  $i = 0;
  while ($i < count($data_set)) {
  $tran = $data_set[$i];
  $local_items = [];
  $j = 0;
  while ($j < count($tran)) {
  $item = $tran[$j];
  if (array_key_exists($item, $header_table)) {
  $local_items = _append($local_items, $item);
}
  $j = $j + 1;
};
  if (count($local_items) > 0) {
  $local_items = sort_items($local_items, $header_table);
  update_tree($local_items, $fp_tree, $header_table, 1);
}
  $i = $i + 1;
};
  return ['tree' => $fp_tree, 'header' => $header_table];
}
function ascend_tree($leaf_node, $path) {
  $prefix = $path;
  if ($leaf_node['parent'] != null) {
  $prefix = _append($prefix, $leaf_node['name']);
  $prefix = ascend_tree($leaf_node['parent'], $prefix);
} else {
  $prefix = _append($prefix, $leaf_node['name']);
}
  return $prefix;
}
function find_prefix_path($base_pat, $tree_node) {
  $cond_pats = [];
  $node = $tree_node;
  while ($node != null) {
  $prefix = ascend_tree($node, []);
  if (count($prefix) > 1) {
  $items = array_slice($prefix, 1, count($prefix) - 1);
  $cond_pats = _append($cond_pats, ['items' => $items, 'count' => $node['count']]);
}
  $node = $node['node_link'];
};
  return $cond_pats;
}
function mine_tree($in_tree, $header_table, $min_sup, $pre_fix, $freq_item_list) {
  $freq_list = $freq_item_list;
  $items = [];
  foreach (array_keys($header_table) as $k) {
  $items = _append($items, $k);
};
  $sorted_items = $items;
  $i = 0;
  while ($i < count($sorted_items)) {
  $j = $i + 1;
  while ($j < count($sorted_items)) {
  if ($header_table[$sorted_items[$i]]['count'] > $header_table[$sorted_items[$j]]['count']) {
  $tmp = $sorted_items[$i];
  $sorted_items[$i] = $sorted_items[$j];
  $sorted_items[$j] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < count($sorted_items)) {
  $base_pat = $sorted_items[$idx];
  $new_freq = $pre_fix;
  $new_freq = _append($new_freq, $base_pat);
  $freq_list = _append($freq_list, $new_freq);
  $cond_pats = find_prefix_path($base_pat, $header_table[$base_pat]['node']);
  $cond_dataset = [];
  $p = 0;
  while ($p < count($cond_pats)) {
  $pat = $cond_pats[$p];
  $r = 0;
  while ($r < $pat['count']) {
  $cond_dataset = _append($cond_dataset, $pat['items']);
  $r = $r + 1;
};
  $p = $p + 1;
};
  $res2 = create_tree($cond_dataset, $min_sup);
  $my_tree = $res2['tree'];
  $my_head = $res2['header'];
  if (_len($my_head) > 0) {
  $freq_list = mine_tree($my_tree, $my_head, $min_sup, $new_freq, $freq_list);
}
  $idx = $idx + 1;
};
  return $freq_list;
}
function list_to_string($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  if ($i < count($xs) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function main() {
  $data_set = [['bread', 'milk', 'cheese'], ['bread', 'milk'], ['bread', 'diapers'], ['bread', 'milk', 'diapers'], ['milk', 'diapers'], ['milk', 'cheese'], ['diapers', 'cheese'], ['bread', 'milk', 'cheese', 'diapers']];
  $res = create_tree($data_set, 3);
  $fp_tree = $res['tree'];
  $header_table = $res['header'];
  $freq_items = [];
  $freq_items = mine_tree($fp_tree, $header_table, 3, [], $freq_items);
  echo rtrim(json_encode(count($data_set), 1344)), PHP_EOL;
  echo rtrim(json_encode(_len($header_table), 1344)), PHP_EOL;
  $i = 0;
  while ($i < count($freq_items)) {
  echo rtrim(list_to_string($freq_items[$i])), PHP_EOL;
  $i = $i + 1;
};
}
main();
