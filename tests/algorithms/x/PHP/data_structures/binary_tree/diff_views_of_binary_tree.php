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
  $NIL = 0 - 1;
  function make_tree() {
  global $NIL, $tree;
  return ['lefts' => [1, $NIL, 3, $NIL, $NIL], 'rights' => [2, $NIL, 4, $NIL, $NIL], 'root' => 0, 'values' => [3, 9, 20, 15, 7]];
};
  function index_of($xs, $x) {
  global $NIL, $tree;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return $i;
}
  $i = $i + 1;
};
  return $NIL;
};
  function sort_pairs(&$hds, &$vals) {
  global $NIL, $tree;
  $i = 0;
  while ($i < count($hds)) {
  $j = $i;
  while ($j > 0 && $hds[$j - 1] > $hds[$j]) {
  $hd_tmp = $hds[$j - 1];
  $hds[$j - 1] = $hds[$j];
  $hds[$j] = $hd_tmp;
  $val_tmp = $vals[$j - 1];
  $vals[$j - 1] = $vals[$j];
  $vals[$j] = $val_tmp;
  $j = $j - 1;
};
  $i = $i + 1;
};
};
  function right_view($t) {
  global $NIL, $tree;
  $res = [];
  $queue = [$t['root']];
  while (count($queue) > 0) {
  $size = count($queue);
  $i = 0;
  while ($i < $size) {
  $idx = $queue[$i];
  if ($t['lefts'][$idx] != $NIL) {
  $queue = _append($queue, $t['lefts'][$idx]);
}
  if ($t['rights'][$idx] != $NIL) {
  $queue = _append($queue, $t['rights'][$idx]);
}
  $i = $i + 1;
};
  $res = _append($res, $t['values'][$queue[$size - 1]]);
  $queue = array_slice($queue, $size, count($queue) - $size);
};
  return $res;
};
  function left_view($t) {
  global $NIL, $tree;
  $res = [];
  $queue = [$t['root']];
  while (count($queue) > 0) {
  $size = count($queue);
  $i = 0;
  while ($i < $size) {
  $idx = $queue[$i];
  if ($t['lefts'][$idx] != $NIL) {
  $queue = _append($queue, $t['lefts'][$idx]);
}
  if ($t['rights'][$idx] != $NIL) {
  $queue = _append($queue, $t['rights'][$idx]);
}
  $i = $i + 1;
};
  $res = _append($res, $t['values'][$queue[0]]);
  $queue = array_slice($queue, $size, count($queue) - $size);
};
  return $res;
};
  function top_view($t) {
  global $NIL, $tree;
  $hds = [];
  $vals = [];
  $queue_idx = [$t['root']];
  $queue_hd = [0];
  while (count($queue_idx) > 0) {
  $idx = $queue_idx[0];
  $queue_idx = array_slice($queue_idx, 1, count($queue_idx) - 1);
  $hd = $queue_hd[0];
  $queue_hd = array_slice($queue_hd, 1, count($queue_hd) - 1);
  if (index_of($hds, $hd) == $NIL) {
  $hds = _append($hds, $hd);
  $vals = _append($vals, $t['values'][$idx]);
}
  if ($t['lefts'][$idx] != $NIL) {
  $queue_idx = _append($queue_idx, $t['lefts'][$idx]);
  $queue_hd = _append($queue_hd, $hd - 1);
}
  if ($t['rights'][$idx] != $NIL) {
  $queue_idx = _append($queue_idx, $t['rights'][$idx]);
  $queue_hd = _append($queue_hd, $hd + 1);
}
};
  sort_pairs($hds, $vals);
  return $vals;
};
  function bottom_view($t) {
  global $NIL, $tree;
  $hds = [];
  $vals = [];
  $queue_idx = [$t['root']];
  $queue_hd = [0];
  while (count($queue_idx) > 0) {
  $idx = $queue_idx[0];
  $queue_idx = array_slice($queue_idx, 1, count($queue_idx) - 1);
  $hd = $queue_hd[0];
  $queue_hd = array_slice($queue_hd, 1, count($queue_hd) - 1);
  $pos = index_of($hds, $hd);
  if ($pos == $NIL) {
  $hds = _append($hds, $hd);
  $vals = _append($vals, $t['values'][$idx]);
} else {
  $vals[$pos] = $t['values'][$idx];
}
  if ($t['lefts'][$idx] != $NIL) {
  $queue_idx = _append($queue_idx, $t['lefts'][$idx]);
  $queue_hd = _append($queue_hd, $hd - 1);
}
  if ($t['rights'][$idx] != $NIL) {
  $queue_idx = _append($queue_idx, $t['rights'][$idx]);
  $queue_hd = _append($queue_hd, $hd + 1);
}
};
  sort_pairs($hds, $vals);
  return $vals;
};
  $tree = make_tree();
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(right_view($tree), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(left_view($tree), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(top_view($tree), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(bottom_view($tree), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
