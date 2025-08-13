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
  function repeat_char($ch, $count) {
  $result = '';
  $i = 0;
  while ($i < $count) {
  $result = $result . $ch;
  $i = $i + 1;
};
  return $result;
};
  function butterfly_pattern($n) {
  $lines = [];
  $i = 1;
  while ($i < $n) {
  $left = repeat_char('*', $i);
  $mid = repeat_char(' ', 2 * ($n - $i) - 1);
  $right = repeat_char('*', $i);
  $lines = _append($lines, $left . $mid . $right);
  $i = $i + 1;
};
  $lines = _append($lines, repeat_char('*', 2 * $n - 1));
  $j = $n - 1;
  while ($j > 0) {
  $left = repeat_char('*', $j);
  $mid = repeat_char(' ', 2 * ($n - $j) - 1);
  $right = repeat_char('*', $j);
  $lines = _append($lines, $left . $mid . $right);
  $j = $j - 1;
};
  $out = '';
  $k = 0;
  while ($k < count($lines)) {
  if ($k > 0) {
  $out = $out . '
';
}
  $out = $out . $lines[$k];
  $k = $k + 1;
};
  return $out;
};
  echo rtrim(butterfly_pattern(3)), PHP_EOL;
  echo rtrim(butterfly_pattern(5)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
