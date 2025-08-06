<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function create_bool_matrix($rows, $cols) {
  $matrix = [];
  $i = 0;
  while ($i <= $rows) {
  $row = [];
  $j = 0;
  while ($j <= $cols) {
  $row = _append($row, false);
  $j = $j + 1;
};
  $matrix = _append($matrix, $row);
  $i = $i + 1;
};
  return $matrix;
}
function is_sum_subset($arr, $required_sum) {
  $arr_len = count($arr);
  $subset = create_bool_matrix($arr_len, $required_sum);
  $i = 0;
  while ($i <= $arr_len) {
  $subset[$i][0] = true;
  $i = $i + 1;
};
  $j = 1;
  while ($j <= $required_sum) {
  $subset[0][$j] = false;
  $j = $j + 1;
};
  $i = 1;
  while ($i <= $arr_len) {
  $j = 1;
  while ($j <= $required_sum) {
  if ($arr[$i - 1] > $j) {
  $subset[$i][$j] = $subset[$i - 1][$j];
}
  if ($arr[$i - 1] <= $j) {
  $subset[$i][$j] = $subset[$i - 1][$j] || $subset[$i - 1][$j - $arr[$i - 1]];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $subset[$arr_len][$required_sum];
}
echo rtrim(json_encode(is_sum_subset([2, 4, 6, 8], 5), 1344)), PHP_EOL;
echo rtrim(json_encode(is_sum_subset([2, 4, 6, 8], 14), 1344)), PHP_EOL;
