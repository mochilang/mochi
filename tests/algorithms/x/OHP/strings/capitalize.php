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
  $lowercase = 'abcdefghijklmnopqrstuvwxyz';
  $uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function index_of($s, $c) {
  global $lowercase, $uppercase;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $c) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function capitalize($sentence) {
  global $lowercase, $uppercase;
  if (strlen($sentence) == 0) {
  return '';
}
  $first = substr($sentence, 0, 1);
  $idx = index_of($lowercase, $first);
  $capital = ($idx >= 0 ? substr($uppercase, $idx, $idx + 1 - $idx) : $first);
  return $capital . substr($sentence, 1, strlen($sentence) - 1);
};
  echo rtrim(capitalize('hello world')), PHP_EOL;
  echo rtrim(capitalize('123 hello world')), PHP_EOL;
  echo rtrim(capitalize(' hello world')), PHP_EOL;
  echo rtrim(capitalize('a')), PHP_EOL;
  echo rtrim(capitalize('')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
