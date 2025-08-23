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
  function get_failure_array($pattern) {
  global $text;
  $failure = [0];
  $i = 0;
  $j = 1;
  while ($j < strlen($pattern)) {
  if (substr($pattern, $i, $i + 1 - $i) == substr($pattern, $j, $j + 1 - $j)) {
  $i = $i + 1;
} else {
  if ($i > 0) {
  $i = $failure[$i - 1];
  continue;
};
}
  $j = $j + 1;
  $failure = _append($failure, $i);
};
  return $failure;
};
  function knuth_morris_pratt($text, $pattern) {
  $failure = get_failure_array($pattern);
  $i = 0;
  $j = 0;
  while ($i < strlen($text)) {
  if (substr($pattern, $j, $j + 1 - $j) == substr($text, $i, $i + 1 - $i)) {
  if ($j == strlen($pattern) - 1) {
  return $i - $j;
};
  $j = $j + 1;
} else {
  if ($j > 0) {
  $j = $failure[$j - 1];
  continue;
};
}
  $i = $i + 1;
};
  return -1;
};
  $text = 'abcxabcdabxabcdabcdabcy';
  $pattern = 'abcdabcy';
  echo rtrim(json_encode(knuth_morris_pratt($text, $pattern), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
