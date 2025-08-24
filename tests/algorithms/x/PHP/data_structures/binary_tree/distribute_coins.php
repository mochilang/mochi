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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function count_nodes($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  return count_nodes($nodes, $node['left']) + count_nodes($nodes, $node['right']) + 1;
};
  function count_coins($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  return count_coins($nodes, $node['left']) + count_coins($nodes, $node['right']) + $node['data'];
};
  $total_moves = 0;
  function iabs($x) {
  global $total_moves;
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function dfs($nodes, $idx) {
  global $total_moves;
  if ($idx == 0) {
  return 0;
}
  $node = $nodes[$idx];
  $left_excess = dfs($nodes, $node['left']);
  $right_excess = dfs($nodes, $node['right']);
  $abs_left = iabs($left_excess);
  $abs_right = iabs($right_excess);
  $total_moves = $total_moves + $abs_left + $abs_right;
  return $node['data'] + $left_excess + $right_excess - 1;
};
  function distribute_coins($nodes, $root) {
  global $total_moves;
  if ($root == 0) {
  return 0;
}
  if (count_nodes($nodes, $root) != count_coins($nodes, $root)) {
  _panic('The nodes number should be same as the number of coins');
}
  $total_moves = 0;
  dfs($nodes, $root);
  return $total_moves;
};
  function main() {
  global $total_moves;
  $example1 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 3, 'left' => 2, 'right' => 3], ['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 0, 'right' => 0]];
  $example2 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 2, 'right' => 3], ['data' => 3, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 0, 'right' => 0]];
  $example3 = [['data' => 0, 'left' => 0, 'right' => 0], ['data' => 0, 'left' => 2, 'right' => 3], ['data' => 0, 'left' => 0, 'right' => 0], ['data' => 3, 'left' => 0, 'right' => 0]];
  echo rtrim(json_encode(distribute_coins($example1, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins($example2, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins($example3, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(distribute_coins([['data' => 0, 'left' => 0, 'right' => 0]], 0), 1344)), PHP_EOL;
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
