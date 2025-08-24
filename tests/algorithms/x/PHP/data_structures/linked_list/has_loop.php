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
  function has_loop($nodes, $head) {
  $slow = $head;
  $fast = $head;
  while ($fast != 0 - 1) {
  $fast_node1 = $nodes[$fast];
  if ($fast_node1['next'] == 0 - 1) {
  return false;
}
  $fast_node2 = $nodes[$fast_node1['next']];
  if ($fast_node2['next'] == 0 - 1) {
  return false;
}
  $slow_node = $nodes[$slow];
  $slow = $slow_node['next'];
  $fast = $fast_node2['next'];
  if ($slow == $fast) {
  return true;
}
};
  return false;
};
  function make_nodes($values) {
  $nodes = [];
  $i = 0;
  while ($i < count($values)) {
  $next_idx = ($i == count($values) - 1 ? 0 - 1 : $i + 1);
  $nodes = _append($nodes, ['data' => $values[$i], 'next' => $next_idx]);
  $i = $i + 1;
};
  return $nodes;
};
  function main() {
  $list1 = make_nodes([1, 2, 3, 4]);
  echo rtrim(_str(has_loop($list1, 0))), PHP_EOL;
  $list1[3]['next'] = 1;
  echo rtrim(_str(has_loop($list1, 0))), PHP_EOL;
  $list2 = make_nodes([5, 6, 5, 6]);
  echo rtrim(_str(has_loop($list2, 0))), PHP_EOL;
  $list3 = make_nodes([1]);
  echo rtrim(_str(has_loop($list3, 0))), PHP_EOL;
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
