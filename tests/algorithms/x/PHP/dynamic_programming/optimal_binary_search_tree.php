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
function sort_nodes($nodes) {
  $arr = $nodes;
  $i = 1;
  while ($i < count($arr)) {
  $key_node = $arr[$i];
  $j = $i - 1;
  while ($j >= 0) {
  $temp = $arr[$j];
  if ($temp['key'] > $key_node['key']) {
  $arr[$j + 1] = $temp;
  $j = $j - 1;
} else {
  break;
}
};
  $arr[$j + 1] = $key_node;
  $i = $i + 1;
};
  return $arr;
}
function print_node($n) {
  echo rtrim('Node(key=' . _str($n['key']) . ', freq=' . _str($n['freq']) . ')'), PHP_EOL;
}
function print_binary_search_tree($root, $keys, $i, $j, $parent, $is_left) {
  if ($i > $j || $i < 0 || $j > count($root) - 1) {
  return;
}
  $node = $root[$i][$j];
  if ($parent == (-1)) {
  echo rtrim(_str($keys[$node]) . ' is the root of the binary search tree.'), PHP_EOL;
} else {
  if ($is_left) {
  echo rtrim(_str($keys[$node]) . ' is the left child of key ' . _str($parent) . '.'), PHP_EOL;
} else {
  echo rtrim(_str($keys[$node]) . ' is the right child of key ' . _str($parent) . '.'), PHP_EOL;
};
}
  print_binary_search_tree($root, $keys, $i, $node - 1, $keys[$node], true);
  print_binary_search_tree($root, $keys, $node + 1, $j, $keys[$node], false);
}
function find_optimal_binary_search_tree($original_nodes) {
  $nodes = sort_nodes($original_nodes);
  $n = count($nodes);
  $keys = [];
  $freqs = [];
  $i = 0;
  while ($i < $n) {
  $node = $nodes[$i];
  $keys = _append($keys, $node['key']);
  $freqs = _append($freqs, $node['freq']);
  $i = $i + 1;
};
  $dp = [];
  $total = [];
  $root = [];
  $i = 0;
  while ($i < $n) {
  $dp_row = [];
  $total_row = [];
  $root_row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $dp_row = _append($dp_row, $freqs[$i]);
  $total_row = _append($total_row, $freqs[$i]);
  $root_row = _append($root_row, $i);
} else {
  $dp_row = _append($dp_row, 0);
  $total_row = _append($total_row, 0);
  $root_row = _append($root_row, 0);
}
  $j = $j + 1;
};
  $dp = _append($dp, $dp_row);
  $total = _append($total, $total_row);
  $root = _append($root, $root_row);
  $i = $i + 1;
};
  $interval_length = 2;
  $INF = 2147483647;
  while ($interval_length <= $n) {
  $i = 0;
  while ($i < $n - $interval_length + 1) {
  $j = $i + $interval_length - 1;
  $dp[$i][$j] = $INF;
  $total[$i][$j] = $total[$i][$j - 1] + $freqs[$j];
  $r = $root[$i][$j - 1];
  while ($r <= $root[$i + 1][$j]) {
  $left = ($r != $i ? $dp[$i][$r - 1] : 0);
  $right = ($r != $j ? $dp[$r + 1][$j] : 0);
  $cost = $left + $total[$i][$j] + $right;
  if ($dp[$i][$j] > $cost) {
  $dp[$i][$j] = $cost;
  $root[$i][$j] = $r;
}
  $r = $r + 1;
};
  $i = $i + 1;
};
  $interval_length = $interval_length + 1;
};
  echo rtrim('Binary search tree nodes:'), PHP_EOL;
  $i = 0;
  while ($i < $n) {
  print_node($nodes[$i]);
  $i = $i + 1;
};
  echo rtrim('
The cost of optimal BST for given tree nodes is ' . _str($dp[0][$n - 1]) . '.'), PHP_EOL;
  print_binary_search_tree($root, $keys, 0, $n - 1, (-1), false);
}
function main() {
  $nodes = [['key' => 12, 'freq' => 8], ['key' => 10, 'freq' => 34], ['key' => 20, 'freq' => 50], ['key' => 42, 'freq' => 3], ['key' => 25, 'freq' => 40], ['key' => 37, 'freq' => 30]];
  find_optimal_binary_search_tree($nodes);
}
main();
