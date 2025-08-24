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
  $NONE = 0 - 1;
  function inorder($tree, $index) {
  global $NONE, $tree1, $tree2, $tree3;
  $res = [];
  if ($index == $NONE) {
  return $res;
}
  $left_idx = $tree['left'][$index];
  if ($left_idx != $NONE) {
  $res = array_merge($res, inorder($tree, $left_idx));
}
  $res = _append($res, $tree['data'][$index]);
  $right_idx = $tree['right'][$index];
  if ($right_idx != $NONE) {
  $res = array_merge($res, inorder($tree, $right_idx));
}
  return $res;
};
  function is_sorted($tree, $index) {
  global $NONE, $tree1, $tree2, $tree3;
  if ($index == $NONE) {
  return true;
}
  $left_idx = $tree['left'][$index];
  if ($left_idx != $NONE) {
  if ($tree['data'][$index] < $tree['data'][$left_idx]) {
  return false;
};
  if (!is_sorted($tree, $left_idx)) {
  return false;
};
}
  $right_idx = $tree['right'][$index];
  if ($right_idx != $NONE) {
  if ($tree['data'][$index] > $tree['data'][$right_idx]) {
  return false;
};
  if (!is_sorted($tree, $right_idx)) {
  return false;
};
}
  return true;
};
  $tree1 = ['data' => [2.1, 2.0, 2.2], 'left' => [1, $NONE, $NONE], 'right' => [2, $NONE, $NONE]];
  echo rtrim('Tree ' . _str(inorder($tree1, 0)) . ' is sorted: ' . _str(is_sorted($tree1, 0))), PHP_EOL;
  $tree2 = ['data' => [2.1, 2.0, 2.0], 'left' => [1, $NONE, $NONE], 'right' => [2, $NONE, $NONE]];
  echo rtrim('Tree ' . _str(inorder($tree2, 0)) . ' is sorted: ' . _str(is_sorted($tree2, 0))), PHP_EOL;
  $tree3 = ['data' => [2.1, 2.0, 2.1], 'left' => [1, $NONE, $NONE], 'right' => [2, $NONE, $NONE]];
  echo rtrim('Tree ' . _str(inorder($tree3, 0)) . ' is sorted: ' . _str(is_sorted($tree3, 0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
