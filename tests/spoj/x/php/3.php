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
  function is_substring($a, $b) {
  global $sample_input;
  $la = strlen($a);
  $lb = strlen($b);
  $i = 0;
  while ($i + $lb <= $la) {
  if (substr($a, $i, $i + $lb - $i) == $b) {
  return 1;
}
  $i = $i + 1;
};
  return 0;
};
  function solve($lines) {
  global $sample_input;
  $res = [];
  foreach ($lines as $line) {
  $parts = [];
  $cur = '';
  $i = 0;
  while ($i < strlen($line)) {
  $ch = substr($line, $i, $i + 1 - $i);
  if ($ch == ' ') {
  $parts = _append($parts, $cur);
  $cur = '';
} else {
  $cur = $cur . $ch;
}
  $i = $i + 1;
};
  $parts = _append($parts, $cur);
  $a = $parts[0];
  $b = $parts[1];
  $res = _append($res, is_substring($a, $b));
};
  return $res;
};
  $sample_input = ['1010110010 10110', '1110111011 10011'];
  foreach (solve($sample_input) as $r) {
  echo json_encode($r, 1344), PHP_EOL;
}
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
