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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function all_rotations($s) {
  global $result;
  $n = strlen($s);
  $rotations = [];
  $i = 0;
  while ($i < $n) {
  $rotation = substr($s, $i, $n - $i) . substr($s, 0, $i);
  $rotations = _append($rotations, $rotation);
  $i = $i + 1;
};
  return $rotations;
};
  function sort_strings($arr) {
  global $result, $s;
  $n = count($arr);
  $i = 1;
  while ($i < $n) {
  $key = $arr[$i];
  $j = $i - 1;
  while ($j >= 0 && $arr[$j] > $key) {
  $arr[$j + 1] = $arr[$j];
  $j = $j - 1;
};
  $arr[$j + 1] = $key;
  $i = $i + 1;
};
  return $arr;
};
  function join_strings($arr) {
  global $result, $s;
  $res = '';
  $i = 0;
  while ($i < count($arr)) {
  $res = $res . $arr[$i];
  $i = $i + 1;
};
  return $res;
};
  function bwt_transform($s) {
  global $result;
  if ($s == '') {
  _panic('input string must not be empty');
}
  $rotations = all_rotations($s);
  $rotations = sort_strings($rotations);
  $last_col = [];
  $i = 0;
  while ($i < count($rotations)) {
  $word = $rotations[$i];
  $last_col = _append($last_col, $word[strlen($word) - 1]);
  $i = $i + 1;
};
  $bwt_string = join_strings($last_col);
  $idx = index_of($rotations, $s);
  return ['bwt_string' => $bwt_string, 'idx_original_string' => $idx];
};
  function index_of($arr, $target) {
  global $result, $s;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $target) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function reverse_bwt($bwt_string, $idx_original_string) {
  global $result, $s;
  if ($bwt_string == '') {
  _panic('bwt string must not be empty');
}
  $n = strlen($bwt_string);
  if ($idx_original_string < 0 || $idx_original_string >= $n) {
  _panic('index out of range');
}
  $ordered_rotations = [];
  $i = 0;
  while ($i < $n) {
  $ordered_rotations = _append($ordered_rotations, '');
  $i = $i + 1;
};
  $iter = 0;
  while ($iter < $n) {
  $j = 0;
  while ($j < $n) {
  $ch = substr($bwt_string, $j, $j + 1 - $j);
  $ordered_rotations[$j] = $ch . $ordered_rotations[$j];
  $j = $j + 1;
};
  $ordered_rotations = sort_strings($ordered_rotations);
  $iter = $iter + 1;
};
  return $ordered_rotations[$idx_original_string];
};
  $s = '^BANANA';
  $result = bwt_transform($s);
  echo rtrim(json_encode($result['bwt_string'], 1344)), PHP_EOL;
  echo rtrim(json_encode($result['idx_original_string'], 1344)), PHP_EOL;
  echo rtrim(reverse_bwt($result['bwt_string'], $result['idx_original_string'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
