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
$__start_mem = memory_get_usage();
$__start = _now();
  function dfs($node, $target, $current) {
  return (function($__v) {
  if ($__v['__tag'] === "Empty") {
    return 0;
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $v = $__v["value"];
    $r = $__v["right"];
    return (($current + $v == $target ? 1 : 0)) + dfs($l, $target, $current + $v) + dfs($r, $target, $current + $v);
  }
})($node);
};
  function path_sum($node, $target) {
  return (function($__v) {
  if ($__v['__tag'] === "Empty") {
    return 0;
  } elseif ($__v['__tag'] === "Node") {
    $l = $__v["left"];
    $v = $__v["value"];
    $r = $__v["right"];
    return dfs($node, $target, 0) + path_sum($l, $target) + path_sum($r, $target);
  }
})($node);
};
  function sample_tree_one() {
  return ['__tag' => 'Node', 'left' => 10, 'right' => ['__tag' => 'Node', 'left' => -3, 'right' => ['__tag' => 'Node', 'left' => 11, 'right' => $Empty, 'value' => $Empty], 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 5, 'right' => ['__tag' => 'Node', 'left' => 2, 'right' => ['__tag' => 'Node', 'left' => 1, 'right' => $Empty, 'value' => $Empty], 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 3, 'right' => ['__tag' => 'Node', 'left' => -2, 'right' => $Empty, 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 3, 'right' => $Empty, 'value' => $Empty]]]];
};
  function sample_tree_two() {
  return ['__tag' => 'Node', 'left' => 10, 'right' => ['__tag' => 'Node', 'left' => -3, 'right' => ['__tag' => 'Node', 'left' => 10, 'right' => $Empty, 'value' => $Empty], 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 5, 'right' => ['__tag' => 'Node', 'left' => 2, 'right' => ['__tag' => 'Node', 'left' => 1, 'right' => $Empty, 'value' => $Empty], 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 3, 'right' => ['__tag' => 'Node', 'left' => -2, 'right' => $Empty, 'value' => $Empty], 'value' => ['__tag' => 'Node', 'left' => 3, 'right' => $Empty, 'value' => $Empty]]]];
};
  function main() {
  $tree1 = sample_tree_one();
  echo rtrim(json_encode(path_sum($tree1, 8), 1344)), PHP_EOL;
  echo rtrim(json_encode(path_sum($tree1, 7), 1344)), PHP_EOL;
  $tree2 = sample_tree_two();
  echo rtrim(json_encode(path_sum($tree2, 8), 1344)), PHP_EOL;
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
