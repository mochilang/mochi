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
$__start_mem = memory_get_usage();
$__start = _now();
  function index_of($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function mochi_upper($word) {
  $lower_chars = 'abcdefghijklmnopqrstuvwxyz';
  $upper_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $result = '';
  $i = 0;
  while ($i < strlen($word)) {
  $c = substr($word, $i, $i + 1 - $i);
  $idx = index_of($lower_chars, $c);
  if ($idx >= 0) {
  $result = $result . substr($upper_chars, $idx, $idx + 1 - $idx);
} else {
  $result = $result . $c;
}
  $i = $i + 1;
};
  return $result;
};
  echo rtrim(strtoupper('wow')), PHP_EOL;
  echo rtrim(strtoupper('Hello')), PHP_EOL;
  echo rtrim(strtoupper('WHAT')), PHP_EOL;
  echo rtrim(strtoupper('wh[]32')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
