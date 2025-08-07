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
  function ugly_numbers($n) {
  if ($n <= 0) {
  return 1;
}
  $ugly_nums = [];
  $ugly_nums = _append($ugly_nums, 1);
  $i2 = 0;
  $i3 = 0;
  $i5 = 0;
  $next_2 = 2;
  $next_3 = 3;
  $next_5 = 5;
  $count = 1;
  while ($count < $n) {
  $next_num = ($next_2 < $next_3 ? ($next_2 < $next_5 ? $next_2 : $next_5) : ($next_3 < $next_5 ? $next_3 : $next_5));
  $ugly_nums = _append($ugly_nums, $next_num);
  if ($next_num == $next_2) {
  $i2 = $i2 + 1;
  $next_2 = $ugly_nums[$i2] * 2;
}
  if ($next_num == $next_3) {
  $i3 = $i3 + 1;
  $next_3 = $ugly_nums[$i3] * 3;
}
  if ($next_num == $next_5) {
  $i5 = $i5 + 1;
  $next_5 = $ugly_nums[$i5] * 5;
}
  $count = $count + 1;
};
  return $ugly_nums[count($ugly_nums) - 1];
};
  echo rtrim(_str(ugly_numbers(100))), PHP_EOL;
  echo rtrim(_str(ugly_numbers(0))), PHP_EOL;
  echo rtrim(_str(ugly_numbers(20))), PHP_EOL;
  echo rtrim(_str(ugly_numbers(-5))), PHP_EOL;
  echo rtrim(_str(ugly_numbers(200))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
