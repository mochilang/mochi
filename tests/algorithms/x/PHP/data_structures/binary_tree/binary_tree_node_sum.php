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
  function node_sum($tree, $index) {
  global $example;
  if ($index == (-1)) {
  return 0;
}
  $node = $tree[$index];
  return $node['value'] + node_sum($tree, $node['left']) + node_sum($tree, $node['right']);
};
  $example = [['left' => 1, 'right' => 2, 'value' => 10], ['left' => 3, 'right' => -1, 'value' => 5], ['left' => 4, 'right' => 5, 'value' => -3], ['left' => -1, 'right' => -1, 'value' => 12], ['left' => -1, 'right' => -1, 'value' => 8], ['left' => -1, 'right' => -1, 'value' => 0]];
  echo rtrim(json_encode(node_sum($example, 0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
