<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function default_matrix_multiplication($a, $b) {
  return [[$a[0][0] * $b[0][0] + $a[0][1] * $b[1][0], $a[0][0] * $b[0][1] + $a[0][1] * $b[1][1]], [$a[1][0] * $b[0][0] + $a[1][1] * $b[1][0], $a[1][0] * $b[0][1] + $a[1][1] * $b[1][1]]];
}
function matrix_addition($matrix_a, $matrix_b) {
  $result = [];
  $i = 0;
  while ($i < count($matrix_a)) {
  $row = [];
  $j = 0;
  while ($j < count($matrix_a[$i])) {
  $row = _append($row, $matrix_a[$i][$j] + $matrix_b[$i][$j]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function matrix_subtraction($matrix_a, $matrix_b) {
  $result = [];
  $i = 0;
  while ($i < count($matrix_a)) {
  $row = [];
  $j = 0;
  while ($j < count($matrix_a[$i])) {
  $row = _append($row, $matrix_a[$i][$j] - $matrix_b[$i][$j]);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
}
function split_matrix($a) {
  $n = count($a);
  $mid = _intdiv($n, 2);
  $top_left = [];
  $top_right = [];
  $bot_left = [];
  $bot_right = [];
  $i = 0;
  while ($i < $mid) {
  $left_row = [];
  $right_row = [];
  $j = 0;
  while ($j < $mid) {
  $left_row = _append($left_row, $a[$i][$j]);
  $right_row = _append($right_row, $a[$i][$j + $mid]);
  $j = $j + 1;
};
  $top_left = _append($top_left, $left_row);
  $top_right = _append($top_right, $right_row);
  $i = $i + 1;
};
  $i = $mid;
  while ($i < $n) {
  $left_row = [];
  $right_row = [];
  $j = 0;
  while ($j < $mid) {
  $left_row = _append($left_row, $a[$i][$j]);
  $right_row = _append($right_row, $a[$i][$j + $mid]);
  $j = $j + 1;
};
  $bot_left = _append($bot_left, $left_row);
  $bot_right = _append($bot_right, $right_row);
  $i = $i + 1;
};
  return [$top_left, $top_right, $bot_left, $bot_right];
}
function matrix_dimensions($matrix) {
  return [count($matrix), count($matrix[0])];
}
function next_power_of_two($n) {
  $p = 1;
  while ($p < $n) {
  $p = $p * 2;
};
  return $p;
}
function pad_matrix($mat, $rows, $cols) {
  $res = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $v = 0;
  if ($i < count($mat) && $j < count($mat[0])) {
  $v = $mat[$i][$j];
}
  $row = _append($row, $v);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
}
function actual_strassen($matrix_a, $matrix_b) {
  if (matrix_dimensions($matrix_a)[0] == 2) {
  return default_matrix_multiplication($matrix_a, $matrix_b);
}
  $parts_a = split_matrix($matrix_a);
  $a = $parts_a[0];
  $b = $parts_a[1];
  $c = $parts_a[2];
  $d = $parts_a[3];
  $parts_b = split_matrix($matrix_b);
  $e = $parts_b[0];
  $f = $parts_b[1];
  $g = $parts_b[2];
  $h = $parts_b[3];
  $t1 = actual_strassen($a, matrix_subtraction($f, $h));
  $t2 = actual_strassen(matrix_addition($a, $b), $h);
  $t3 = actual_strassen(matrix_addition($c, $d), $e);
  $t4 = actual_strassen($d, matrix_subtraction($g, $e));
  $t5 = actual_strassen(matrix_addition($a, $d), matrix_addition($e, $h));
  $t6 = actual_strassen(matrix_subtraction($b, $d), matrix_addition($g, $h));
  $t7 = actual_strassen(matrix_subtraction($a, $c), matrix_addition($e, $f));
  $top_left = matrix_addition(matrix_subtraction(matrix_addition($t5, $t4), $t2), $t6);
  $top_right = matrix_addition($t1, $t2);
  $bot_left = matrix_addition($t3, $t4);
  $bot_right = matrix_subtraction(matrix_subtraction(matrix_addition($t1, $t5), $t3), $t7);
  $new_matrix = [];
  $i = 0;
  while ($i < count($top_right)) {
  $new_matrix = _append($new_matrix, array_merge($top_left[$i], $top_right[$i]));
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($bot_right)) {
  $new_matrix = _append($new_matrix, array_merge($bot_left[$i], $bot_right[$i]));
  $i = $i + 1;
};
  return $new_matrix;
}
function strassen($matrix1, $matrix2) {
  $dims1 = matrix_dimensions($matrix1);
  $dims2 = matrix_dimensions($matrix2);
  if ($dims1[1] != $dims2[0]) {
  return [];
}
  $maximum = intval(max([$dims1[0], $dims1[1], $dims2[0], $dims2[1]]));
  $size = next_power_of_two($maximum);
  $new_matrix1 = pad_matrix($matrix1, $size, $size);
  $new_matrix2 = pad_matrix($matrix2, $size, $size);
  $result_padded = actual_strassen($new_matrix1, $new_matrix2);
  $final_matrix = [];
  $i = 0;
  while ($i < $dims1[0]) {
  $row = [];
  $j = 0;
  while ($j < $dims2[1]) {
  $row = _append($row, $result_padded[$i][$j]);
  $j = $j + 1;
};
  $final_matrix = _append($final_matrix, $row);
  $i = $i + 1;
};
  return $final_matrix;
}
function main() {
  $matrix1 = [[2, 3, 4, 5], [6, 4, 3, 1], [2, 3, 6, 7], [3, 1, 2, 4], [2, 3, 4, 5], [6, 4, 3, 1], [2, 3, 6, 7], [3, 1, 2, 4], [2, 3, 4, 5], [6, 2, 3, 1]];
  $matrix2 = [[0, 2, 1, 1], [16, 2, 3, 3], [2, 2, 7, 7], [13, 11, 22, 4]];
  $res = strassen($matrix1, $matrix2);
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($res, 1344))))))), PHP_EOL;
}
main();
