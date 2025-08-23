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
  function split_words($s) {
  $words = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if (strlen($current) > 0) {
  $words = _append($words, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  if (strlen($current) > 0) {
  $words = _append($words, $current);
}
  return $words;
};
  function reverse_words($input_str) {
  $words = split_words($input_str);
  $res = '';
  $i = count($words) - 1;
  while ($i >= 0) {
  $res = $res . $words[$i];
  if ($i > 0) {
  $res = $res . ' ';
}
  $i = $i - 1;
};
  return $res;
};
  function main() {
  echo rtrim(reverse_words('I love Python')), PHP_EOL;
  echo rtrim(reverse_words('I     Love          Python')), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
