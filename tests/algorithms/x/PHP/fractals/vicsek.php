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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function repeat_char($c, $count) {
  $s = '';
  $i = 0;
  while ($i < $count) {
  $s = $s . $c;
  $i = $i + 1;
};
  return $s;
};
  function vicsek($order) {
  if ($order == 0) {
  return ['#'];
}
  $prev = vicsek($order - 1);
  $size = count($prev);
  $blank = repeat_char(' ', $size);
  $result = [];
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $blank . $prev[$i] . $blank);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $prev[$i] . $prev[$i] . $prev[$i]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < $size) {
  $result = _append($result, $blank . $prev[$i] . $blank);
  $i = $i + 1;
};
  return $result;
};
  function print_pattern($pattern) {
  $i = 0;
  while ($i < count($pattern)) {
  echo rtrim($pattern[$i]), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  $depth = 3;
  $pattern = vicsek($depth);
  print_pattern($pattern);
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
