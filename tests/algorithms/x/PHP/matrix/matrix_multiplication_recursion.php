<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function is_square($matrix) {
  global $matrix_1_to_4, $matrix_5_to_8, $matrix_count_up, $matrix_unordered;
  $n = count($matrix);
  $i = 0;
  while ($i < $n) {
  if (count($matrix[$i]) != $n) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function matrix_multiply($a, $b) {
  global $matrix_1_to_4, $matrix_5_to_8, $matrix_count_up, $matrix_unordered;
  $rows = count($a);
  $cols = count($b[0]);
  $inner = count($b);
  $result = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $sum = 0;
  $k = 0;
  while ($k < $inner) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function multiply($i, $j, $k, $a, $b, &$result, $n, $m) {
  global $matrix_1_to_4, $matrix_5_to_8, $matrix_count_up, $matrix_unordered;
  if ($i >= $n) {
  return;
}
  if ($j >= $m) {
  multiply($i + 1, 0, 0, $a, $b, $result, $n, $m);
  return;
}
  if ($k >= count($b)) {
  multiply($i, $j + 1, 0, $a, $b, $result, $n, $m);
  return;
}
  $result[$i][$j] = $result[$i][$j] + $a[$i][$k] * $b[$k][$j];
  multiply($i, $j, $k + 1, $a, $b, $result, $n, $m);
}
function matrix_multiply_recursive($a, $b) {
  global $matrix_1_to_4, $matrix_5_to_8, $matrix_count_up, $matrix_unordered;
  if (count($a) == 0 || count($b) == 0) {
  return [];
}
  if (count($a) != count($b) || (!is_square($a)) || (!is_square($b))) {
  _panic('Invalid matrix dimensions');
}
  $n = count($a);
  $m = count($b[0]);
  $result = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $m) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  multiply(0, 0, 0, $a, $b, $result, $n, $m);
  return $result;
}
$matrix_1_to_4 = [[1, 2], [3, 4]];
$matrix_5_to_8 = [[5, 6], [7, 8]];
$matrix_count_up = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]];
$matrix_unordered = [[5, 8, 1, 2], [6, 7, 3, 0], [4, 5, 9, 1], [2, 6, 10, 14]];
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(matrix_multiply_recursive($matrix_1_to_4, $matrix_5_to_8), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(matrix_multiply_recursive($matrix_count_up, $matrix_unordered), 1344)))))), PHP_EOL;
