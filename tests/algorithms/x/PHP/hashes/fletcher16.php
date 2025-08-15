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
  $ascii_chars = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  function mochi_ord($ch) {
  global $ascii_chars;
  $i = 0;
  while ($i < strlen($ascii_chars)) {
  if (substr($ascii_chars, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
};
  function fletcher16($text) {
  global $ascii_chars;
  $sum1 = 0;
  $sum2 = 0;
  $i = 0;
  while ($i < strlen($text)) {
  $code = mochi_ord(substr($text, $i, $i + 1 - $i));
  $sum1 = ($sum1 + $code) % 255;
  $sum2 = ($sum1 + $sum2) % 255;
  $i = $i + 1;
};
  return $sum2 * 256 + $sum1;
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
