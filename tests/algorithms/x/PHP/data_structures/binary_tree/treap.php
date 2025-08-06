<?php
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
  $node_values = [];
  $node_priors = [];
  $node_lefts = [];
  $node_rights = [];
  $seed = 1;
  function random() {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  $seed = ($seed * 13 + 7) % 100;
  return (floatval($seed)) / 100.0;
};
  function new_node($value) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  $node_values = _append($node_values, $value);
  $node_priors = _append($node_priors, random());
  $node_lefts = _append($node_lefts, $NIL);
  $node_rights = _append($node_rights, $NIL);
  return count($node_values) - 1;
};
  function split($root, $value) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  if ($root == $NIL) {
  return ['left' => $NIL, 'right' => $NIL];
}
  if ($value < $node_values[$root]) {
  $res = explode($value, $node_lefts[$root]);
  $node_lefts[$root] = $res['right'];
  return ['left' => $res['left'], 'right' => $root];
}
  $res = explode($value, $node_rights[$root]);
  $node_rights[$root] = $res['left'];
  return ['left' => $root, 'right' => $res['right']];
};
  function merge($left, $right) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  if ($left == $NIL) {
  return $right;
}
  if ($right == $NIL) {
  return $left;
}
  if ($node_priors[$left] < $node_priors[$right]) {
  $node_rights[$left] = merge($node_rights[$left], $right);
  return $left;
}
  $node_lefts[$right] = merge($left, $node_lefts[$right]);
  return $right;
};
  function insert($root, $value) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  $node = new_node($value);
  $res = explode($value, $root);
  return merge(merge($res['left'], $node), $res['right']);
};
  function erase($root, $value) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  $res1 = explode($value - 1, $root);
  $res2 = explode($value, $res1['right']);
  return merge($res1['left'], $res2['right']);
};
  function inorder($i, $acc) {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  if ($i == $NIL) {
  return $acc;
}
  $left_acc = inorder($node_lefts[$i], $acc);
  $with_node = _append($left_acc, $node_values[$i]);
  return inorder($node_rights[$i], $with_node);
};
  function main() {
  global $NIL, $node_values, $node_priors, $node_lefts, $node_rights, $seed;
  $root = $NIL;
  $root = insert($root, 1);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
  $root = insert($root, 3);
  $root = insert($root, 5);
  $root = insert($root, 17);
  $root = insert($root, 19);
  $root = insert($root, 2);
  $root = insert($root, 16);
  $root = insert($root, 4);
  $root = insert($root, 0);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
  $root = insert($root, 4);
  $root = insert($root, 4);
  $root = insert($root, 4);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
  $root = erase($root, 0);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
  $root = erase($root, 4);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
