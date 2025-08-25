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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_parent_idx($idx) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return _intdiv(($idx - 1), 2);
};
  function get_left_child_idx($idx) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return $idx * 2 + 1;
};
  function get_right_child_idx($idx) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return $idx * 2 + 2;
};
  function remove_key($m, $k) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  $out = [];
  foreach (array_keys($m) as $key) {
  if ($key != $k) {
  $out[$key] = $m[$key];
}
};
  return $out;
};
  function slice_without_last($xs) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
};
  function sift_down(&$mh, $idx) {
  global $a, $b, $e, $my_min_heap, $r, $x;
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
};
  function sift_up(&$mh, $idx) {
  global $a, $b, $e, $my_min_heap, $r, $x;
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
};
  function new_min_heap($array) {
  global $a, $b, $e, $my_min_heap, $r, $x;
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
  $mh = ['heap' => $heap, 'heap_dict' => $val_map, 'idx_of_element' => $idx_map];
  $start = get_parent_idx(count($array) - 1);
  while ($start >= 0) {
  sift_down($mh, $start);
  $start = $start - 1;
};
  return $mh;
};
  function peek($mh) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return $mh['heap'][0];
};
  function remove_min(&$mh) {
  global $a, $b, $e, $my_min_heap, $r, $x;
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
};
  function insert(&$mh, $node) {
  global $a, $b, $e, $my_min_heap, $r, $x;
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
};
  function is_empty($mh) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return count($mh['heap']) == 0;
};
  function get_value($mh, $key) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return $mh['heap_dict'][$key];
};
  function decrease_key(&$mh, &$node, $new_value) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  $heap = $mh['heap'];
  $val_map = $mh['heap_dict'];
  $idx_map = $mh['idx_of_element'];
  $idx = $idx_map[$node['name']];
  if (!($heap[$idx]['val'] > $new_value)) {
  _panic('newValue must be less than current value');
}
  $node['val'] = $new_value;
  $heap[$idx]['val'] = $new_value;
  $val_map[$node['name']] = $new_value;
  $mh['heap'] = $heap;
  $mh['heap_dict'] = $val_map;
  sift_up($mh, $idx);
};
  function node_to_string($n) {
  global $a, $b, $e, $my_min_heap, $r, $x;
  return 'Node(' . $n['name'] . ', ' . _str($n['val']) . ')';
};
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
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
