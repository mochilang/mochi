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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_repeat($s, $n) {
  global $records;
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $s;
  $i = $i + 1;
};
  return $out;
};
  $records = [repeat('abcdefgh', 10), repeat('ijklmnop', 10), repeat('qrstuvwx', 10)];
  function reverseStr($s) {
  global $records;
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
};
  foreach ($records as $r) {
  echo rtrim(reverseStr($r)), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
