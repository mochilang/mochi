<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function all_rotations($s) {
  global $result;
  $n = strlen($s);
  $rotations = [];
  $i = 0;
  while ($i < $n) {
  $rotation = substr($s, $i, $n - $i) . substr($s, 0, $i - 0);
  $rotations = _append($rotations, $rotation);
  $i = $i + 1;
};
  return $rotations;
}
function sort_strings(&$arr) {
  global $s, $result;
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
}
function join_strings($arr) {
  global $s, $result;
  $res = '';
  $i = 0;
  while ($i < count($arr)) {
  $res = $res . $arr[$i];
  $i = $i + 1;
};
  return $res;
}
function bwt_transform($s) {
  global $result;
  if ($s == '') {
  $panic('input string must not be empty');
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
}
function index_of($arr, $target) {
  global $s, $result;
  $i = 0;
  while ($i < count($arr)) {
  if ($arr[$i] == $target) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function reverse_bwt($bwt_string, $idx_original_string) {
  global $s, $result;
  if ($bwt_string == '') {
  $panic('bwt string must not be empty');
}
  $n = strlen($bwt_string);
  if ($idx_original_string < 0 || $idx_original_string >= $n) {
  $panic('index out of range');
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
}
$s = '^BANANA';
$result = bwt_transform($s);
echo rtrim(json_encode($result['bwt_string'], 1344)), PHP_EOL;
echo rtrim(json_encode($result['idx_original_string'], 1344)), PHP_EOL;
echo rtrim(reverse_bwt($result['bwt_string'], $result['idx_original_string'])), PHP_EOL;
