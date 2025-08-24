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
  $NULL = 0 - 1;
  function empty_list() {
  global $NULL;
  return ['head' => $NULL, 'next' => []];
};
  function add_node($list, $value) {
  global $NULL;
  $nexts = $list['next'];
  $new_index = count($nexts);
  $nexts = _append($nexts, $NULL);
  if ($list['head'] == $NULL) {
  return ['head' => $new_index, 'next' => $nexts];
}
  $last = $list['head'];
  while ($nexts[$last] != $NULL) {
  $last = $nexts[$last];
};
  $new_nexts = [];
  $i = 0;
  while ($i < count($nexts)) {
  if ($i == $last) {
  $new_nexts = _append($new_nexts, $new_index);
} else {
  $new_nexts = _append($new_nexts, $nexts[$i]);
}
  $i = $i + 1;
};
  return ['head' => $list['head'], 'next' => $new_nexts];
};
  function set_next($list, $index, $next_index) {
  global $NULL;
  $nexts = $list['next'];
  $new_nexts = [];
  $i = 0;
  while ($i < count($nexts)) {
  if ($i == $index) {
  $new_nexts = _append($new_nexts, $next_index);
} else {
  $new_nexts = _append($new_nexts, $nexts[$i]);
}
  $i = $i + 1;
};
  return ['head' => $list['head'], 'next' => $new_nexts];
};
  function detect_cycle($list) {
  global $NULL;
  if ($list['head'] == $NULL) {
  return false;
}
  $nexts = $list['next'];
  $slow = $list['head'];
  $fast = $list['head'];
  while ($fast != $NULL && $nexts[$fast] != $NULL) {
  $slow = $nexts[$slow];
  $fast = $nexts[$nexts[$fast]];
  if ($slow == $fast) {
  return true;
}
};
  return false;
};
  function main() {
  global $NULL;
  $ll = empty_list();
  $ll = add_node($ll, 1);
  $ll = add_node($ll, 2);
  $ll = add_node($ll, 3);
  $ll = add_node($ll, 4);
  $ll = set_next($ll, 3, 1);
  echo rtrim(json_encode(detect_cycle($ll), 1344)), PHP_EOL;
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
