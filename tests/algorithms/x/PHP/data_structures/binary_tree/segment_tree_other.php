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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function combine($a, $b, $op) {
  global $arr, $tree;
  if ($op == 0) {
  return $a + $b;
}
  if ($op == 1) {
  if ($a > $b) {
  return $a;
};
  return $b;
}
  if ($a < $b) {
  return $a;
}
  return $b;
}
function build_tree($nodes, $arr, $start, $end, $op) {
  global $tree;
  if ($start == $end) {
  $node = ['start' => $start, 'end' => $end, 'val' => $arr[$start], 'mid' => $start, 'left' => -1, 'right' => -1];
  $new_nodes = _append($nodes, $node);
  return ['nodes' => $new_nodes, 'idx' => count($new_nodes) - 1];
}
  $mid = _intdiv(($start + $end), 2);
  $left_res = build_tree($nodes, $arr, $start, $mid, $op);
  $right_res = build_tree($left_res['nodes'], $arr, $mid + 1, $end, $op);
  $left_node = $right_res['nodes'][$left_res['idx']];
  $right_node = $right_res['nodes'][$right_res['idx']];
  $val = combine($left_node['val'], $right_node['val'], $op);
  $parent = ['start' => $start, 'end' => $end, 'val' => $val, 'mid' => $mid, 'left' => $left_res['idx'], 'right' => $right_res['idx']];
  $new_nodes = _append($right_res['nodes'], $parent);
  return ['nodes' => $new_nodes, 'idx' => count($new_nodes) - 1];
}
function new_segment_tree($collection, $op) {
  global $arr, $tree;
  return ['arr' => $collection, 'op' => $op];
}
function update($tree, $i, $val) {
  global $arr;
  $new_arr = [];
  $idx = 0;
  while ($idx < _len($tree['arr'])) {
  if ($idx == $i) {
  $new_arr = _append($new_arr, $val);
} else {
  $new_arr = _append($new_arr, $tree[$arr][$idx]);
}
  $idx = $idx + 1;
};
  return ['arr' => $new_arr, 'op' => $tree['op']];
}
function query_range($tree, $i, $j) {
  global $arr;
  $result = $tree['arr'][$i];
  $idx = $i + 1;
  while ($idx <= $j) {
  $result = combine($result, $tree['arr'][$idx], $tree['op']);
  $idx = $idx + 1;
};
  return $result;
}
function traverse($tree) {
  global $arr;
  if (_len($tree['arr']) == 0) {
  return [];
}
  $res = build_tree([], $tree['arr'], 0, _len($tree['arr']) - 1, $tree['op']);
  return $res['nodes'];
}
function node_to_string($node) {
  global $arr, $tree;
  return 'SegmentTreeNode(start=' . _str($node['start']) . ', end=' . _str($node['end']) . ', val=' . _str($node['val']) . ')';
}
function print_traverse($tree) {
  global $arr;
  $nodes = traverse($tree);
  $i = 0;
  while ($i < count($nodes)) {
  echo rtrim(node_to_string($nodes[$i])), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
}
$arr = [2, 1, 5, 3, 4];
foreach ([0, 1, 2] as $op) {
  echo rtrim('**************************************************'), PHP_EOL;
  $tree = new_segment_tree($arr, $op);
  print_traverse($tree);
  $tree = update($tree, 1, 5);
  print_traverse($tree);
  echo rtrim(json_encode(query_range($tree, 3, 4), 1344)), PHP_EOL;
  echo rtrim(json_encode(query_range($tree, 2, 2), 1344)), PHP_EOL;
  echo rtrim(json_encode(query_range($tree, 1, 3), 1344)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
}
