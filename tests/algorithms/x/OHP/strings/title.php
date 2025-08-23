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
  $lower = 'abcdefghijklmnopqrstuvwxyz';
  $upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function index_of($s, $ch) {
  global $lower, $upper;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function to_title_case($word) {
  global $lower, $upper;
  if (strlen($word) == 0) {
  return '';
}
  $first = substr($word, 0, 1);
  $idx = index_of($lower, $first);
  $result = ($idx >= 0 ? substr($upper, $idx, $idx + 1 - $idx) : $first);
  $i = 1;
  while ($i < strlen($word)) {
  $ch = substr($word, $i, $i + 1 - $i);
  $uidx = index_of($upper, $ch);
  if ($uidx >= 0) {
  $result = $result . substr($lower, $uidx, $uidx + 1 - $uidx);
} else {
  $result = $result . $ch;
}
  $i = $i + 1;
};
  return $result;
};
  function split_words($s) {
  global $lower, $upper;
  $words = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == ' ') {
  if (strlen($current) > 0) {
  $words = _append($words, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  if (strlen($current) > 0) {
  $words = _append($words, $current);
}
  return $words;
};
  function sentence_to_title_case($sentence) {
  global $lower, $upper;
  $words = split_words($sentence);
  $res = '';
  $i = 0;
  while ($i < count($words)) {
  $res = $res . to_title_case($words[$i]);
  if ($i + 1 < count($words)) {
  $res = $res . ' ';
}
  $i = $i + 1;
};
  return $res;
};
  echo rtrim(to_title_case('Aakash')), PHP_EOL;
  echo rtrim(to_title_case('aakash')), PHP_EOL;
  echo rtrim(to_title_case('AAKASH')), PHP_EOL;
  echo rtrim(to_title_case('aAkAsH')), PHP_EOL;
  echo rtrim(sentence_to_title_case('Aakash Giri')), PHP_EOL;
  echo rtrim(sentence_to_title_case('aakash giri')), PHP_EOL;
  echo rtrim(sentence_to_title_case('AAKASH GIRI')), PHP_EOL;
  echo rtrim(sentence_to_title_case('aAkAsH gIrI')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
