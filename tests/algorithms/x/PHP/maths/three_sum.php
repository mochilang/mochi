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
$__start_mem = memory_get_usage();
$__start = _now();
  function bubble_sort($nums) {
  $arr = $nums;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $temp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $temp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function three_sum($nums) {
  $sorted = bubble_sort($nums);
  $res = [];
  $n = count($sorted);
  $i = 0;
  while ($i < $n - 2) {
  if ($i == 0 || $sorted[$i] != $sorted[$i - 1]) {
  $low = $i + 1;
  $high = $n - 1;
  $c = 0 - $sorted[$i];
  while ($low < $high) {
  $s = $sorted[$low] + $sorted[$high];
  if ($s == $c) {
  $triple = [$sorted[$i], $sorted[$low], $sorted[$high]];
  $res = _append($res, $triple);
  while ($low < $high && $sorted[$low] == $sorted[$low + 1]) {
  $low = $low + 1;
};
  while ($low < $high && $sorted[$high] == $sorted[$high - 1]) {
  $high = $high - 1;
};
  $low = $low + 1;
  $high = $high - 1;
} else {
  if ($s < $c) {
  $low = $low + 1;
} else {
  $high = $high - 1;
};
}
};
}
  $i = $i + 1;
};
  return $res;
};
  echo rtrim(_str(three_sum([-1, 0, 1, 2, -1, -4]))), PHP_EOL;
  echo rtrim(_str(three_sum([1, 2, 3, 4]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
