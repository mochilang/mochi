<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if ($x === null) { return 0; }
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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
function new_node(&$state, $value) {
  $state['nodes'] = _append($state['nodes'], [$value => $value, 'left' => (-1), 'right' => (-1)]);
  return _isub(_len($state['nodes']), 1);
}
function insert(&$state, $value) {
  if ($state['root'] == (-1)) {
  $state['root'] = new_node($state, $value);
  return;
}
  $current = $state['root'];
  $nodes = $state['nodes'];
  while (true) {
  $node = $nodes[$current];
  if ($value < $node['value']) {
  if ($node['left'] == (-1)) {
  $idx = new_node($state, $value);
  $nodes = $state['nodes'];
  $node['left'] = $idx;
  $nodes[$current] = $node;
  $state['nodes'] = $nodes;
  return;
};
  $current = $node['left'];
} else {
  if ($value > $node['value']) {
  if ($node['right'] == (-1)) {
  $idx = new_node($state, $value);
  $nodes = $state['nodes'];
  $node['right'] = $idx;
  $nodes[$current] = $node;
  $state['nodes'] = $nodes;
  return;
};
  $current = $node['right'];
} else {
  return;
};
}
};
}
function inorder($state, $idx) {
  if ($idx == (-1)) {
  return [];
}
  $node = $state['nodes'][$idx];
  $result = inorder($state, $node['left']);
  $result = _append($result, $node['value']);
  $right_part = inorder($state, $node['right']);
  $i = 0;
  while ($i < count($right_part)) {
  $result = _append($result, $right_part[$i]);
  $i = _iadd($i, 1);
};
  return $result;
}
function tree_sort($arr) {
  $state = ['nodes' => [], 'root' => (-1)];
  $i = 0;
  while ($i < count($arr)) {
  insert($state, $arr[$i]);
  $i = _iadd($i, 1);
};
  if ($state['root'] == (-1)) {
  return [];
}
  return inorder($state, $state['root']);
}
echo rtrim(_str(tree_sort([]))), PHP_EOL;
echo rtrim(_str(tree_sort([1]))), PHP_EOL;
echo rtrim(_str(tree_sort([1, 2]))), PHP_EOL;
echo rtrim(_str(tree_sort([5, 2, 7]))), PHP_EOL;
echo rtrim(_str(tree_sort([5, -4, 9, 2, 7]))), PHP_EOL;
echo rtrim(_str(tree_sort([5, 6, 1, -1, 4, 37, 2, 7]))), PHP_EOL;
