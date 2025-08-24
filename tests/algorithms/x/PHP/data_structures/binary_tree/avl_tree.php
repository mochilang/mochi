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
$__start_mem = memory_get_usage();
$__start = _now();
  $NIL = 0 - 1;
  $nodes = [];
  function new_node($value) {
  global $NIL, $nodes;
  $node = ['data' => $value, 'height' => 1, 'left' => $NIL, 'right' => $NIL];
  $nodes = _append($nodes, $node);
  return count($nodes) - 1;
};
  function get_height($i) {
  global $NIL, $nodes;
  if ($i == $NIL) {
  return 0;
}
  return $nodes[$i]['height'];
};
  function my_max($a, $b) {
  global $NIL, $nodes;
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function update_height($i) {
  global $NIL, $nodes;
  $nodes[$i]['height'] = my_max(get_height($nodes[$i]['left']), get_height($nodes[$i]['right'])) + 1;
};
  function right_rotation($i) {
  global $NIL, $nodes;
  $left = $nodes[$i]['left'];
  $nodes[$i]['left'] = $nodes[$left]['right'];
  $nodes[$left]['right'] = $i;
  update_height($i);
  update_height($left);
  return $left;
};
  function left_rotation($i) {
  global $NIL, $nodes;
  $right = $nodes[$i]['right'];
  $nodes[$i]['right'] = $nodes[$right]['left'];
  $nodes[$right]['left'] = $i;
  update_height($i);
  update_height($right);
  return $right;
};
  function lr_rotation($i) {
  global $NIL, $nodes;
  $nodes[$i]['left'] = left_rotation($nodes[$i]['left']);
  return right_rotation($i);
};
  function rl_rotation($i) {
  global $NIL, $nodes;
  $nodes[$i]['right'] = right_rotation($nodes[$i]['right']);
  return left_rotation($i);
};
  function insert_node($i, $value) {
  global $NIL, $nodes;
  if ($i == $NIL) {
  return new_node($value);
}
  if ($value < $nodes[$i]['data']) {
  $nodes[$i]['left'] = insert_node($nodes[$i]['left'], $value);
  if (get_height($nodes[$i]['left']) - get_height($nodes[$i]['right']) == 2) {
  if ($value < $nodes[$nodes[$i]['left']]['data']) {
  $i = right_rotation($i);
} else {
  $i = lr_rotation($i);
};
};
} else {
  $nodes[$i]['right'] = insert_node($nodes[$i]['right'], $value);
  if (get_height($nodes[$i]['right']) - get_height($nodes[$i]['left']) == 2) {
  if ($value < $nodes[$nodes[$i]['right']]['data']) {
  $i = rl_rotation($i);
} else {
  $i = left_rotation($i);
};
};
}
  update_height($i);
  return $i;
};
  function get_left_most($i) {
  global $NIL, $nodes;
  $cur = $i;
  while ($nodes[$cur]['left'] != $NIL) {
  $cur = $nodes[$cur]['left'];
};
  return $nodes[$cur]['data'];
};
  function del_node($i, $value) {
  global $NIL, $nodes;
  if ($i == $NIL) {
  return $NIL;
}
  if ($value < $nodes[$i]['data']) {
  $nodes[$i]['left'] = del_node($nodes[$i]['left'], $value);
} else {
  if ($value > $nodes[$i]['data']) {
  $nodes[$i]['right'] = del_node($nodes[$i]['right'], $value);
} else {
  if ($nodes[$i]['left'] != $NIL && $nodes[$i]['right'] != $NIL) {
  $temp = get_left_most($nodes[$i]['right']);
  $nodes[$i]['data'] = $temp;
  $nodes[$i]['right'] = del_node($nodes[$i]['right'], $temp);
} else {
  if ($nodes[$i]['left'] != $NIL) {
  $i = $nodes[$i]['left'];
} else {
  $i = $nodes[$i]['right'];
};
};
};
}
  if ($i == $NIL) {
  return $NIL;
}
  $lh = get_height($nodes[$i]['left']);
  $rh = get_height($nodes[$i]['right']);
  if ($rh - $lh == 2) {
  if (get_height($nodes[$nodes[$i]['right']]['right']) > get_height($nodes[$nodes[$i]['right']]['left'])) {
  $i = left_rotation($i);
} else {
  $i = rl_rotation($i);
};
} else {
  if ($lh - $rh == 2) {
  if (get_height($nodes[$nodes[$i]['left']]['left']) > get_height($nodes[$nodes[$i]['left']]['right'])) {
  $i = right_rotation($i);
} else {
  $i = lr_rotation($i);
};
};
}
  update_height($i);
  return $i;
};
  function inorder($i) {
  global $NIL, $nodes;
  if ($i == $NIL) {
  return '';
}
  $left = inorder($nodes[$i]['left']);
  $right = inorder($nodes[$i]['right']);
  $res = _str($nodes[$i]['data']);
  if ($left != '') {
  $res = $left . ' ' . $res;
}
  if ($right != '') {
  $res = $res . ' ' . $right;
}
  return $res;
};
  function main() {
  global $NIL, $nodes;
  $nodes = [];
  $root = $NIL;
  $root = insert_node($root, 4);
  $root = insert_node($root, 2);
  $root = insert_node($root, 3);
  echo rtrim(inorder($root)), PHP_EOL;
  echo rtrim(_str(get_height($root))), PHP_EOL;
  $root = del_node($root, 3);
  echo rtrim(inorder($root)), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
