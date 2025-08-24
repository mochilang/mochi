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
  function inorder($nodes, $idx) {
  global $tree;
  if ($idx == (-1)) {
  return [];
}
  $node = $nodes[$idx];
  $result = inorder($nodes, $node['left']);
  $result = _append($result, $node['key']);
  $result = array_merge($result, inorder($nodes, $node['right']));
  return $result;
};
  function floor_ceiling($nodes, $idx, $key) {
  global $tree;
  $floor_val = null;
  $ceiling_val = null;
  $current = $idx;
  while ($current != (-1)) {
  $node = $nodes[$current];
  if ($node['key'] == $key) {
  $floor_val = $node['key'];
  $ceiling_val = $node['key'];
  break;
}
  if ($key < $node['key']) {
  $ceiling_val = $node['key'];
  $current = $node['left'];
} else {
  $floor_val = $node['key'];
  $current = $node['right'];
}
};
  return [$floor_val, $ceiling_val];
};
  $tree = [['key' => 10, 'left' => 1, 'right' => 2], ['key' => 5, 'left' => 3, 'right' => 4], ['key' => 20, 'left' => 5, 'right' => 6], ['key' => 3, 'left' => -1, 'right' => -1], ['key' => 7, 'left' => -1, 'right' => -1], ['key' => 15, 'left' => -1, 'right' => -1], ['key' => 25, 'left' => -1, 'right' => -1]];
  echo rtrim(_str(inorder($tree, 0))), PHP_EOL;
  echo rtrim(_str(floor_ceiling($tree, 0, 8))), PHP_EOL;
  echo rtrim(_str(floor_ceiling($tree, 0, 14))), PHP_EOL;
  echo rtrim(_str(floor_ceiling($tree, 0, -1))), PHP_EOL;
  echo rtrim(_str(floor_ceiling($tree, 0, 30))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
