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
  function match_in_pattern($pat, $ch) {
  $i = strlen($pat) - 1;
  while ($i >= 0) {
  if (substr($pat, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i - 1;
};
  return -1;
};
  function mismatch_in_text($text, $pat, $current_pos) {
  $i = strlen($pat) - 1;
  while ($i >= 0) {
  if (substr($pat, $i, $i + 1 - $i) != substr($text, $current_pos + $i, $current_pos + $i + 1 - ($current_pos + $i))) {
  return $current_pos + $i;
}
  $i = $i - 1;
};
  return -1;
};
  function bad_character_heuristic($text, $pat) {
  $textLen = strlen($text);
  $patLen = strlen($pat);
  $positions = [];
  $i = 0;
  while ($i <= $textLen - $patLen) {
  $mismatch_index = mismatch_in_text($text, $pat, $i);
  if ($mismatch_index < 0) {
  $positions = _append($positions, $i);
  $i = $i + 1;
} else {
  $ch = substr($text, $mismatch_index, $mismatch_index + 1 - $mismatch_index);
  $match_index = match_in_pattern($pat, $ch);
  if ($match_index < 0) {
  $i = $mismatch_index + 1;
} else {
  $i = $mismatch_index - $match_index;
};
}
};
  return $positions;
};
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
