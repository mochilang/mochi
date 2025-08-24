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
  function create_node($value) {
  return [$value, null, null];
};
  function insert($node, $value) {
  if ($node == null) {
  return create_node($value);
}
  if ($value < $node[0]) {
  $node[1] = insert($node[1], $value);
} else {
  if ($value > $node[0]) {
  $node[2] = insert($node[2], $value);
};
}
  return $node;
};
  function search($node, $value) {
  if ($node == null) {
  return false;
}
  if ($value == $node[0]) {
  return true;
}
  if ($value < $node[0]) {
  return search($node[1], $value);
}
  return search($node[2], $value);
};
  function inorder($node, $acc) {
  if ($node == null) {
  return $acc;
}
  $left_acc = inorder($node[1], $acc);
  $with_node = _append($left_acc, $node[0]);
  return inorder($node[2], $with_node);
};
  function find_min($node) {
  $current = $node;
  while ($current[1] != null) {
  $current = $current[1];
};
  return $current[0];
};
  function find_max($node) {
  $current = $node;
  while ($current[2] != null) {
  $current = $current[2];
};
  return $current[0];
};
  function delete($node, $value) {
  if ($node == null) {
  return null;
}
  if ($value < $node[0]) {
  $node[1] = delete($node[1], $value);
} else {
  if ($value > $node[0]) {
  $node[2] = delete($node[2], $value);
} else {
  if ($node[1] == null) {
  return $node[2];
};
  if ($node[2] == null) {
  return $node[1];
};
  $min_val = find_min($node[2]);
  $node[0] = $min_val;
  $node[2] = delete($node[2], $min_val);
};
}
  return $node;
};
  function main() {
  $root = null;
  $nums = [8, 3, 6, 1, 10, 14, 13, 4, 7];
  foreach ($nums as $v) {
  $root = insert($root, $v);
};
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
  echo rtrim(json_encode(search($root, 6), 1344)), PHP_EOL;
  echo rtrim(json_encode(search($root, 20), 1344)), PHP_EOL;
  echo rtrim(json_encode(find_min($root), 1344)), PHP_EOL;
  echo rtrim(json_encode(find_max($root), 1344)), PHP_EOL;
  $root = delete($root, 6);
  echo rtrim(_str(inorder($root, []))), PHP_EOL;
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
