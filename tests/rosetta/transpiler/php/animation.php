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
$__start_mem = memory_get_usage();
$__start = _now();
  $msg = 'Hello World! ';
  $shift = 0;
  $inc = 1;
  $clicks = 0;
  $frames = 0;
  while ($clicks < 5) {
  $line = '';
  $i = 0;
  while ($i < strlen($msg)) {
  $idx = fmod(($shift + $i), strlen($msg));
  $line = $line . substr($msg, $idx, $idx + 1 - $idx);
  $i = $i + 1;
};
  echo rtrim($line), PHP_EOL;
  $shift = fmod(($shift + $inc), strlen($msg));
  $frames = $frames + 1;
  if (fmod($frames, strlen($msg)) == 0) {
  $inc = strlen($msg) - $inc;
  $clicks = $clicks + 1;
}
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
