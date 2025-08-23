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
  function mochi_split($s, $sep) {
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function capitalize($word) {
  if (strlen($word) == 0) {
  return '';
}
  $first = strtoupper(substr($word, 0, 1));
  $rest = substr($word, 1, strlen($word) - 1);
  return $first . $rest;
};
  function snake_to_camel_case($input_str, $use_pascal) {
  $words = mochi_split($input_str, '_');
  $result = '';
  $index = 0;
  if (!$use_pascal) {
  if (count($words) > 0) {
  $result = $words[0];
  $index = 1;
};
}
  while ($index < count($words)) {
  $word = $words[$index];
  $result = $result . capitalize($word);
  $index = $index + 1;
};
  return $result;
};
  echo rtrim(snake_to_camel_case('some_random_string', false)), PHP_EOL;
  echo rtrim(snake_to_camel_case('some_random_string', true)), PHP_EOL;
  echo rtrim(snake_to_camel_case('some_random_string_with_numbers_123', false)), PHP_EOL;
  echo rtrim(snake_to_camel_case('some_random_string_with_numbers_123', true)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
