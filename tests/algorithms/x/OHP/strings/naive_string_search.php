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
  function naive_string_search($text, $pattern) {
  $pat_len = strlen($pattern);
  $positions = [];
  $i = 0;
  while ($i <= strlen($text) - $pat_len) {
  $match_found = true;
  $j = 0;
  while ($j < $pat_len) {
  if (substr($text, $i + $j, $i + $j + 1 - ($i + $j)) != substr($pattern, $j, $j + 1 - $j)) {
  $match_found = false;
  break;
}
  $j = $j + 1;
};
  if ($match_found) {
  $positions = _append($positions, $i);
}
  $i = $i + 1;
};
  return $positions;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(naive_string_search('ABAAABCDBBABCDDEBCABC', 'ABC'), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
