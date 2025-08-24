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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function inorder($nodes, $index, $acc) {
  global $medium, $small;
  if ($index == 0 - 1) {
  return $acc;
}
  $node = $nodes[$index];
  $res = inorder($nodes, $node['left'], $acc);
  $res = _append($res, $node['data']);
  $res = inorder($nodes, $node['right'], $res);
  return $res;
};
  function size($nodes, $index) {
  global $medium, $small;
  if ($index == 0 - 1) {
  return 0;
}
  $node = $nodes[$index];
  return 1 + size($nodes, $node['left']) + size($nodes, $node['right']);
};
  function depth($nodes, $index) {
  global $medium, $small;
  if ($index == 0 - 1) {
  return 0;
}
  $node = $nodes[$index];
  $left_depth = depth($nodes, $node['left']);
  $right_depth = depth($nodes, $node['right']);
  if ($left_depth > $right_depth) {
  return $left_depth + 1;
}
  return $right_depth + 1;
};
  function is_full($nodes, $index) {
  global $medium, $small;
  if ($index == 0 - 1) {
  return true;
}
  $node = $nodes[$index];
  if ($node['left'] == 0 - 1 && $node['right'] == 0 - 1) {
  return true;
}
  if ($node['left'] != 0 - 1 && $node['right'] != 0 - 1) {
  return is_full($nodes, $node['left']) && is_full($nodes, $node['right']);
}
  return false;
};
  function small_tree() {
  global $medium, $small;
  $arr = [];
  $arr = _append($arr, ['data' => 2, 'left' => 1, 'right' => 2]);
  $arr = _append($arr, ['data' => 1, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 3, 'left' => 0 - 1, 'right' => 0 - 1]);
  return $arr;
};
  function medium_tree() {
  global $medium, $small;
  $arr = [];
  $arr = _append($arr, ['data' => 4, 'left' => 1, 'right' => 4]);
  $arr = _append($arr, ['data' => 2, 'left' => 2, 'right' => 3]);
  $arr = _append($arr, ['data' => 1, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 3, 'left' => 0 - 1, 'right' => 0 - 1]);
  $arr = _append($arr, ['data' => 5, 'left' => 0 - 1, 'right' => 5]);
  $arr = _append($arr, ['data' => 6, 'left' => 0 - 1, 'right' => 6]);
  $arr = _append($arr, ['data' => 7, 'left' => 0 - 1, 'right' => 0 - 1]);
  return $arr;
};
  $small = small_tree();
  echo rtrim(json_encode(size($small, 0), 1344)), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inorder($small, 0, []), 1344)))))), PHP_EOL;
  echo rtrim(json_encode(depth($small, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_full($small, 0), 1344)), PHP_EOL;
  $medium = medium_tree();
  echo rtrim(json_encode(size($medium, 0), 1344)), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inorder($medium, 0, []), 1344)))))), PHP_EOL;
  echo rtrim(json_encode(depth($medium, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(is_full($medium, 0), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
