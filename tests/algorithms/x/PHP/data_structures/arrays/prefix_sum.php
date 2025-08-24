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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function make_prefix_sum($arr) {
  global $ps, $ps2;
  $prefix = [];
  $running = 0;
  $i = 0;
  while ($i < count($arr)) {
  $running = $running + $arr[$i];
  $prefix = _append($prefix, $running);
  $i = $i + 1;
};
  return ['prefix_sum' => $prefix];
};
  function get_sum($ps, $start, $end) {
  global $ps2;
  $prefix = $ps['prefix_sum'];
  if (count($prefix) == 0) {
  _panic('The array is empty.');
}
  if ($start < 0 || $end >= count($prefix) || $start > $end) {
  _panic('Invalid range specified.');
}
  if ($start == 0) {
  return $prefix[$end];
}
  return $prefix[$end] - $prefix[$start - 1];
};
  function contains_sum($ps, $target_sum) {
  global $ps2;
  $prefix = $ps['prefix_sum'];
  $sums = [0];
  $i = 0;
  while ($i < count($prefix)) {
  $sum_item = $prefix[$i];
  $j = 0;
  while ($j < count($sums)) {
  if ($sums[$j] == $sum_item - $target_sum) {
  return true;
}
  $j = $j + 1;
};
  $sums = _append($sums, $sum_item);
  $i = $i + 1;
};
  return false;
};
  $ps = make_prefix_sum([1, 2, 3]);
  echo rtrim(_str(get_sum($ps, 0, 2))), PHP_EOL;
  echo rtrim(_str(get_sum($ps, 1, 2))), PHP_EOL;
  echo rtrim(_str(get_sum($ps, 2, 2))), PHP_EOL;
  echo rtrim(_str(contains_sum($ps, 6))), PHP_EOL;
  echo rtrim(_str(contains_sum($ps, 5))), PHP_EOL;
  echo rtrim(_str(contains_sum($ps, 3))), PHP_EOL;
  echo rtrim(_str(contains_sum($ps, 4))), PHP_EOL;
  echo rtrim(_str(contains_sum($ps, 7))), PHP_EOL;
  $ps2 = make_prefix_sum([1, -2, 3]);
  echo rtrim(_str(contains_sum($ps2, 2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
