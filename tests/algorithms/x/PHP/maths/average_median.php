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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
  $a = $arr[$j];
  $b = $arr[$j + 1];
  if ($a > $b) {
  $arr[$j] = $b;
  $arr[$j + 1] = $a;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function median($nums) {
  $sorted_list = bubble_sort($nums);
  $length = count($sorted_list);
  $mid_index = _intdiv($length, 2);
  if ($length % 2 == 0) {
  return (floatval(($sorted_list[$mid_index] + $sorted_list[$mid_index - 1]))) / 2.0;
} else {
  return floatval($sorted_list[$mid_index]);
}
};
  echo rtrim(_str(median([0]))), PHP_EOL;
  echo rtrim(_str(median([4, 1, 3, 2]))), PHP_EOL;
  echo rtrim(_str(median([2, 70, 6, 50, 20, 8, 4]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
