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
function get_parent_idx($idx) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return _intdiv(($idx - 1), 2);
}
function get_left_child_idx($idx) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return $idx * 2 + 1;
}
function get_right_child_idx($idx) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return $idx * 2 + 2;
}
function remove_key($m, $k) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
}
function slice_without_last($xs) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function sift_down(&$mh, $idx) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $heap = $mh['heap'];
  $idx_map = $mh['idx_of_element'];
  $i = $idx;
  while (true) {
  $left = get_left_child_idx($i);
  $right = get_right_child_idx($i);
  $smallest = $i;
  if ($left < count($heap) && $heap[$left]['val'] < $heap[$smallest]['val']) {
  $smallest = $left;
}
  if ($right < count($heap) && $heap[$right]['val'] < $heap[$smallest]['val']) {
  $smallest = $right;
}
  if ($smallest != $i) {
  $tmp = $heap[$i];
  $heap[$i] = $heap[$smallest];
  $heap[$smallest] = $tmp;
  $idx_map[$heap[$i]['name']] = $i;
  $idx_map[$heap[$smallest]['name']] = $smallest;
  $i = $smallest;
} else {
  break;
}
};
  $mh['heap'] = $heap;
  $mh['idx_of_element'] = $idx_map;
}
function sift_up(&$mh, $idx) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $heap = $mh['heap'];
  $idx_map = $mh['idx_of_element'];
  $i = $idx;
  $p = get_parent_idx($i);
  while ($p >= 0 && $heap[$p]['val'] > $heap[$i]['val']) {
  $tmp = $heap[$p];
  $heap[$p] = $heap[$i];
  $heap[$i] = $tmp;
  $idx_map[$heap[$p]['name']] = $p;
  $idx_map[$heap[$i]['name']] = $i;
  $i = $p;
  $p = get_parent_idx($i);
};
  $mh['heap'] = $heap;
  $mh['idx_of_element'] = $idx_map;
}
function new_min_heap($array) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $idx_map = [];
  $val_map = [];
  $heap = $array;
  $i = 0;
  while ($i < count($array)) {
  $n = $array[$i];
  $idx_map[$n['name']] = $i;
  $val_map[$n['name']] = $n['val'];
  $i = $i + 1;
};
  $mh = ['heap' => $heap, 'idx_of_element' => $idx_map, 'heap_dict' => $val_map];
  $start = get_parent_idx(count($array) - 1);
  while ($start >= 0) {
  sift_down($mh, $start);
  $start = $start - 1;
};
  return $mh;
}
function peek($mh) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return $mh['heap'][0];
}
function remove_min(&$mh) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $heap = $mh['heap'];
  $idx_map = $mh['idx_of_element'];
  $val_map = $mh['heap_dict'];
  $last_idx = count($heap) - 1;
  $top = $heap[0];
  $last = $heap[$last_idx];
  $heap[0] = $last;
  $idx_map[$last['name']] = 0;
  $heap = slice_without_last($heap);
  $idx_map = remove_key($idx_map, $top['name']);
  $val_map = remove_key($val_map, $top['name']);
  $mh['heap'] = $heap;
  $mh['idx_of_element'] = $idx_map;
  $mh['heap_dict'] = $val_map;
  if (count($heap) > 0) {
  sift_down($mh, 0);
}
  return $top;
}
function insert(&$mh, $node) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $heap = $mh['heap'];
  $idx_map = $mh['idx_of_element'];
  $val_map = $mh['heap_dict'];
  $heap = _append($heap, $node);
  $idx = count($heap) - 1;
  $idx_map[$node['name']] = $idx;
  $val_map[$node['name']] = $node['val'];
  $mh['heap'] = $heap;
  $mh['idx_of_element'] = $idx_map;
  $mh['heap_dict'] = $val_map;
  sift_up($mh, $idx);
}
function is_empty($mh) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return _len($mh['heap']) == 0;
}
function get_value($mh, $key) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return $mh['heap_dict'][$key];
}
function decrease_key(&$mh, &$node, $new_value) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  $heap = $mh['heap'];
  $val_map = $mh['heap_dict'];
  $idx_map = $mh['idx_of_element'];
  $idx = $idx_map[$node['name']];
  if (!($heap[$idx]['val'] > $new_value)) {
  $panic('newValue must be less than current value');
}
  $node['val'] = $new_value;
  $heap[$idx]['val'] = $new_value;
  $val_map[$node['name']] = $new_value;
  $mh['heap'] = $heap;
  $mh['heap_dict'] = $val_map;
  sift_up($mh, $idx);
}
function node_to_string($n) {
  global $r, $b, $a, $x, $e, $my_min_heap;
  return 'Node(' . $n['name'] . ', ' . _str($n['val']) . ')';
}
$r = ['name' => 'R', 'val' => -1];
$b = ['name' => 'B', 'val' => 6];
$a = ['name' => 'A', 'val' => 3];
$x = ['name' => 'X', 'val' => 1];
$e = ['name' => 'E', 'val' => 4];
$my_min_heap = new_min_heap([$r, $b, $a, $x, $e]);
echo rtrim('Min Heap - before decrease key'), PHP_EOL;
foreach ($my_min_heap['heap'] as $n) {
  echo rtrim(node_to_string($n)), PHP_EOL;
}
echo rtrim('Min Heap - After decrease key of node [B -> -17]'), PHP_EOL;
decrease_key($my_min_heap, $b, -17);
foreach ($my_min_heap['heap'] as $n) {
  echo rtrim(node_to_string($n)), PHP_EOL;
}
echo rtrim(_str(get_value($my_min_heap, 'B'))), PHP_EOL;
