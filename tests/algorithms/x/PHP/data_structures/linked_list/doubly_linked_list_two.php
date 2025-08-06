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
function empty_list() {
  return ['nodes' => [], 'head_idx' => -1, 'tail_idx' => -1];
}
function get_head_data($ll) {
  if ($ll['head_idx'] == (-1)) {
  return -1;
}
  $node = $ll['nodes'][$ll['head_idx']];
  return $node['data'];
}
function get_tail_data($ll) {
  if ($ll['tail_idx'] == (-1)) {
  return -1;
}
  $node = $ll['nodes'][$ll['tail_idx']];
  return $node['data'];
}
function insert_before_node(&$ll, $idx, $new_idx) {
  $nodes = $ll['nodes'];
  $new_node = $nodes[$new_idx];
  $new_node['next_index'] = $idx;
  $node = $nodes[$idx];
  $p = $node['prev_index'];
  $new_node['prev_index'] = $p;
  $nodes[$new_idx] = $new_node;
  if ($p == (-1)) {
  $ll['head_idx'] = $new_idx;
} else {
  $prev_node = $nodes[$p];
  $prev_node['next_index'] = $new_idx;
  $nodes[$p] = $prev_node;
}
  $node['prev_index'] = $new_idx;
  $nodes[$idx] = $node;
  $ll['nodes'] = $nodes;
}
function insert_after_node(&$ll, $idx, $new_idx) {
  $nodes = $ll['nodes'];
  $new_node = $nodes[$new_idx];
  $new_node['prev_index'] = $idx;
  $node = $nodes[$idx];
  $nxt = $node['next_index'];
  $new_node['next_index'] = $nxt;
  $nodes[$new_idx] = $new_node;
  if ($nxt == (-1)) {
  $ll['tail_idx'] = $new_idx;
} else {
  $next_node = $nodes[$nxt];
  $next_node['prev_index'] = $new_idx;
  $nodes[$nxt] = $next_node;
}
  $node['next_index'] = $new_idx;
  $nodes[$idx] = $node;
  $ll['nodes'] = $nodes;
}
function set_head(&$ll, $idx) {
  if ($ll['head_idx'] == (-1)) {
  $ll['head_idx'] = $idx;
  $ll['tail_idx'] = $idx;
} else {
  insert_before_node($ll, $ll['head_idx'], $idx);
}
}
function set_tail(&$ll, $idx) {
  if ($ll['tail_idx'] == (-1)) {
  $ll['head_idx'] = $idx;
  $ll['tail_idx'] = $idx;
} else {
  insert_after_node($ll, $ll['tail_idx'], $idx);
}
}
function insert(&$ll, $value) {
  $nodes = $ll['nodes'];
  $nodes = _append($nodes, ['data' => $value, 'prev_index' => -1, 'next_index' => -1]);
  $idx = count($nodes) - 1;
  $ll['nodes'] = $nodes;
  if ($ll['head_idx'] == (-1)) {
  $ll['head_idx'] = $idx;
  $ll['tail_idx'] = $idx;
} else {
  insert_after_node($ll, $ll['tail_idx'], $idx);
}
}
function insert_at_position(&$ll, $position, $value) {
  $current = $ll['head_idx'];
  $current_pos = 1;
  while ($current != (-1)) {
  if ($current_pos == $position) {
  $nodes = $ll['nodes'];
  $nodes = _append($nodes, ['data' => $value, 'prev_index' => -1, 'next_index' => -1]);
  $new_idx = count($nodes) - 1;
  $ll['nodes'] = $nodes;
  insert_before_node($ll, $current, $new_idx);
  return;
}
  $node = $ll['nodes'][$current];
  $current = $node['next_index'];
  $current_pos = $current_pos + 1;
};
  insert($ll, $value);
}
function get_node($ll, $item) {
  $current = $ll['head_idx'];
  while ($current != (-1)) {
  $node = $ll['nodes'][$current];
  if ($node['data'] == $item) {
  return $current;
}
  $current = $node['next_index'];
};
  return -1;
}
function remove_node_pointers(&$ll, $idx) {
  $nodes = $ll['nodes'];
  $node = $nodes[$idx];
  $nxt = $node['next_index'];
  $p = $node['prev_index'];
  if ($nxt != (-1)) {
  $nxt_node = $nodes[$nxt];
  $nxt_node['prev_index'] = $p;
  $nodes[$nxt] = $nxt_node;
}
  if ($p != (-1)) {
  $prev_node = $nodes[$p];
  $prev_node['next_index'] = $nxt;
  $nodes[$p] = $prev_node;
}
  $node['next_index'] = -1;
  $node['prev_index'] = -1;
  $nodes[$idx] = $node;
  $ll['nodes'] = $nodes;
}
function delete_value(&$ll, $value) {
  $idx = get_node($ll, $value);
  if ($idx == (-1)) {
  return;
}
  if ($idx == $ll['head_idx']) {
  $node = $ll['nodes'][$idx];
  $ll['head_idx'] = $node['next_index'];
}
  if ($idx == $ll['tail_idx']) {
  $node = $ll['nodes'][$idx];
  $ll['tail_idx'] = $node['prev_index'];
}
  remove_node_pointers($ll, $idx);
}
function contains($ll, $value) {
  return get_node($ll, $value) != (-1);
}
function is_empty($ll) {
  return $ll['head_idx'] == (-1);
}
function to_string($ll) {
  $res = '';
  $first = true;
  $current = $ll['head_idx'];
  while ($current != (-1)) {
  $node = $ll['nodes'][$current];
  $val = _str($node['data']);
  if ($first) {
  $res = $val;
  $first = false;
} else {
  $res = $res . ' ' . $val;
}
  $current = $node['next_index'];
};
  return $res;
}
function print_list($ll) {
  $current = $ll['head_idx'];
  while ($current != (-1)) {
  $node = $ll['nodes'][$current];
  echo rtrim(_str($node['data'])), PHP_EOL;
  $current = $node['next_index'];
};
}
function main() {
  $ll = empty_list();
  echo rtrim(_str(get_head_data($ll))), PHP_EOL;
  echo rtrim(_str(get_tail_data($ll))), PHP_EOL;
  echo rtrim(_str(is_empty($ll))), PHP_EOL;
  insert($ll, 10);
  echo rtrim(_str(get_head_data($ll))), PHP_EOL;
  echo rtrim(_str(get_tail_data($ll))), PHP_EOL;
  insert_at_position($ll, 3, 20);
  echo rtrim(_str(get_head_data($ll))), PHP_EOL;
  echo rtrim(_str(get_tail_data($ll))), PHP_EOL;
  $nodes = $ll['nodes'];
  $nodes = _append($nodes, ['data' => 1000, 'prev_index' => -1, 'next_index' => -1]);
  $idx_head = count($nodes) - 1;
  $ll['nodes'] = $nodes;
  set_head($ll, $idx_head);
  $nodes = $ll['nodes'];
  $nodes = _append($nodes, ['data' => 2000, 'prev_index' => -1, 'next_index' => -1]);
  $idx_tail = count($nodes) - 1;
  $ll['nodes'] = $nodes;
  set_tail($ll, $idx_tail);
  print_list($ll);
  echo rtrim(_str(is_empty($ll))), PHP_EOL;
  print_list($ll);
  echo rtrim(_str(contains($ll, 10))), PHP_EOL;
  delete_value($ll, 10);
  echo rtrim(_str(contains($ll, 10))), PHP_EOL;
  delete_value($ll, 2000);
  echo rtrim(_str(get_tail_data($ll))), PHP_EOL;
  delete_value($ll, 1000);
  echo rtrim(_str(get_tail_data($ll))), PHP_EOL;
  echo rtrim(_str(get_head_data($ll))), PHP_EOL;
  print_list($ll);
  delete_value($ll, 20);
  print_list($ll);
  $i = 1;
  while ($i < 10) {
  insert($ll, $i);
  $i = $i + 1;
};
  print_list($ll);
  $ll2 = empty_list();
  insert_at_position($ll2, 1, 10);
  echo rtrim(to_string($ll2)), PHP_EOL;
  insert_at_position($ll2, 2, 20);
  echo rtrim(to_string($ll2)), PHP_EOL;
  insert_at_position($ll2, 1, 30);
  echo rtrim(to_string($ll2)), PHP_EOL;
  insert_at_position($ll2, 3, 40);
  echo rtrim(to_string($ll2)), PHP_EOL;
  insert_at_position($ll2, 5, 50);
  echo rtrim(to_string($ll2)), PHP_EOL;
}
main();
