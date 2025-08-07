<?php
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function bubble_sort($nums) {
  $arr = [];
  $i = 0;
  while ($i < count($nums)) {
  $arr = _append($arr, $nums[$i]);
  $i = $i + 1;
};
  $n = count($arr);
  $a = 0;
  while ($a < $n) {
  $b = 0;
  while ($b < $n - $a - 1) {
  if ($arr[$b] > $arr[$b + 1]) {
  $tmp = $arr[$b];
  $arr[$b] = $arr[$b + 1];
  $arr[$b + 1] = $tmp;
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  return $arr;
};
  function sort3($xs) {
  $arr = [];
  $i = 0;
  while ($i < count($xs)) {
  $arr = _append($arr, $xs[$i]);
  $i = $i + 1;
};
  $n = count($arr);
  $a = 0;
  while ($a < $n) {
  $b = 0;
  while ($b < $n - $a - 1) {
  if ($arr[$b] > $arr[$b + 1]) {
  $tmp = $arr[$b];
  $arr[$b] = $arr[$b + 1];
  $arr[$b + 1] = $tmp;
}
  $b = $b + 1;
};
  $a = $a + 1;
};
  return $arr;
};
  function triplet_sum1($arr, $target) {
  $i = 0;
  while ($i < count($arr) - 2) {
  $j = $i + 1;
  while ($j < count($arr) - 1) {
  $k = $j + 1;
  while ($k < count($arr)) {
  if ($arr[$i] + $arr[$j] + $arr[$k] == $target) {
  return sort3([$arr[$i], $arr[$j], $arr[$k]]);
}
  $k = $k + 1;
};
  $j = $j + 1;
};
  $i = $i + 1;
};
  return [0, 0, 0];
};
  function triplet_sum2($arr, $target) {
  $sorted = bubble_sort($arr);
  $n = count($sorted);
  $i = 0;
  while ($i < $n - 2) {
  $left = $i + 1;
  $right = $n - 1;
  while ($left < $right) {
  $s = $sorted[$i] + $sorted[$left] + $sorted[$right];
  if ($s == $target) {
  return [$sorted[$i], $sorted[$left], $sorted[$right]];
}
  if ($s < $target) {
  $left = $left + 1;
} else {
  $right = $right - 1;
}
};
  $i = $i + 1;
};
  return [0, 0, 0];
};
  function list_equal($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function test_triplet_sum() {
  $arr1 = [13, 29, 7, 23, 5];
  if (!list_equal(triplet_sum1($arr1, 35), [5, 7, 23])) {
  _panic('ts1 case1 failed');
}
  if (!list_equal(triplet_sum2($arr1, 35), [5, 7, 23])) {
  _panic('ts2 case1 failed');
}
  $arr2 = [37, 9, 19, 50, 44];
  if (!list_equal(triplet_sum1($arr2, 65), [9, 19, 37])) {
  _panic('ts1 case2 failed');
}
  if (!list_equal(triplet_sum2($arr2, 65), [9, 19, 37])) {
  _panic('ts2 case2 failed');
}
  $arr3 = [6, 47, 27, 1, 15];
  if (!list_equal(triplet_sum1($arr3, 11), [0, 0, 0])) {
  _panic('ts1 case3 failed');
}
  if (!list_equal(triplet_sum2($arr3, 11), [0, 0, 0])) {
  _panic('ts2 case3 failed');
}
};
  function main() {
  test_triplet_sum();
  $sample = [13, 29, 7, 23, 5];
  $res = triplet_sum2($sample, 35);
  echo rtrim(_str($res[0]) . ' ' . _str($res[1]) . ' ' . _str($res[2])), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
