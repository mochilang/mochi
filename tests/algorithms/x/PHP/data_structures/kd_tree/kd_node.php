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
  function make_kd_node($point, $left, $right) {
  global $left_child, $nodes, $right_child, $root;
  return ['left' => $left, 'point' => $point, 'right' => $right];
};
  $nodes = [];
  $nodes = _append($nodes, make_kd_node([2.0, 3.0], 1, 2));
  $nodes = _append($nodes, make_kd_node([1.0, 5.0], -1, -1));
  $nodes = _append($nodes, make_kd_node([4.0, 2.0], -1, -1));
  $root = $nodes[0];
  $left_child = $nodes[1];
  $right_child = $nodes[2];
  echo rtrim(_str($root['point'])), PHP_EOL;
  echo rtrim(_str($root['left'])), PHP_EOL;
  echo rtrim(_str($root['right'])), PHP_EOL;
  echo rtrim(_str($left_child['point'])), PHP_EOL;
  echo rtrim(_str($right_child['point'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
