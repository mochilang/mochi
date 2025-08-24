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
  function max_list($xs) {
  global $ds;
  $m = $xs[0];
  $i = 1;
  while ($i < count($xs)) {
  if ($xs[$i] > $m) {
  $m = $xs[$i];
}
  $i = $i + 1;
};
  return $m;
};
  function disjoint_set_new($set_counts) {
  global $ds;
  $max_set = max_list($set_counts);
  $num_sets = count($set_counts);
  $ranks = [];
  $parents = [];
  $i = 0;
  while ($i < $num_sets) {
  $ranks = _append($ranks, 1);
  $parents = _append($parents, $i);
  $i = $i + 1;
};
  return ['max_set' => $max_set, 'parents' => $parents, 'ranks' => $ranks, 'set_counts' => $set_counts];
};
  function get_parent(&$ds, $idx) {
  if ($ds['parents'][$idx] == $idx) {
  return $idx;
}
  $parents = $ds['parents'];
  $parents[$idx] = get_parent($ds, $parents[$idx]);
  $ds['parents'] = $parents;
  return $ds['parents'][$idx];
};
  function merge(&$ds, $src, $dst) {
  $src_parent = get_parent($ds, $src);
  $dst_parent = get_parent($ds, $dst);
  if ($src_parent == $dst_parent) {
  return false;
}
  if ($ds['ranks'][$dst_parent] >= $ds['ranks'][$src_parent]) {
  $counts = $ds['set_counts'];
  $counts[$dst_parent] = $counts[$dst_parent] + $counts[$src_parent];
  $counts[$src_parent] = 0;
  $ds['set_counts'] = $counts;
  $parents = $ds['parents'];
  $parents[$src_parent] = $dst_parent;
  $ds['parents'] = $parents;
  if ($ds['ranks'][$dst_parent] == $ds['ranks'][$src_parent]) {
  $ranks = $ds['ranks'];
  $ranks[$dst_parent] = $ranks[$dst_parent] + 1;
  $ds['ranks'] = $ranks;
};
  $joined = $ds['set_counts'][$dst_parent];
  if ($joined > $ds['max_set']) {
  $ds['max_set'] = $joined;
};
} else {
  $counts = $ds['set_counts'];
  $counts[$src_parent] = $counts[$src_parent] + $counts[$dst_parent];
  $counts[$dst_parent] = 0;
  $ds['set_counts'] = $counts;
  $parents = $ds['parents'];
  $parents[$dst_parent] = $src_parent;
  $ds['parents'] = $parents;
  $joined = $ds['set_counts'][$src_parent];
  if ($joined > $ds['max_set']) {
  $ds['max_set'] = $joined;
};
}
  return true;
};
  $ds = disjoint_set_new([1, 1, 1]);
  echo rtrim(json_encode(merge($ds, 1, 2), 1344)), PHP_EOL;
  echo rtrim(json_encode(merge($ds, 0, 2), 1344)), PHP_EOL;
  echo rtrim(json_encode(merge($ds, 0, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode(get_parent($ds, 0), 1344)), PHP_EOL;
  echo rtrim(json_encode(get_parent($ds, 1), 1344)), PHP_EOL;
  echo rtrim(json_encode($ds['max_set'], 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
