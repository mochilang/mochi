<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
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
}
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
  return ['set_counts' => $set_counts, 'max_set' => $max_set, 'ranks' => $ranks, 'parents' => $parents];
}
function get_parent(&$ds, $idx) {
  if ($ds['parents'][$idx] == $idx) {
  return $idx;
}
  $parents = $ds['parents'];
  $parents[$idx] = get_parent($ds, $parents[$idx]);
  $ds['parents'] = $parents;
  return $ds['parents'][$idx];
}
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
}
$ds = disjoint_set_new([1, 1, 1]);
echo rtrim(json_encode(merge($ds, 1, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(merge($ds, 0, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(merge($ds, 0, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(get_parent($ds, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(get_parent($ds, 1), 1344)), PHP_EOL;
echo rtrim(json_encode($ds['max_set'], 1344)), PHP_EOL;
