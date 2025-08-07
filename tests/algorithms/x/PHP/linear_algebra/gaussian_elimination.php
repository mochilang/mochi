<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function retroactive_resolution($coefficients, $vector) {
  $rows = count($coefficients);
  $x = [];
  $i = 0;
  while ($i < $rows) {
  $inner = [];
  $inner = _append($inner, 0.0);
  $x = _append($x, $inner);
  $i = $i + 1;
};
  $r = $rows - 1;
  while ($r >= 0) {
  $total = 0.0;
  $c = $r + 1;
  while ($c < $rows) {
  $total = $total + $coefficients[$r][$c] * $x[$c][0];
  $c = $c + 1;
};
  $x[$r][0] = ($vector[$r][0] - $total) / $coefficients[$r][$r];
  $r = $r - 1;
};
  return $x;
}
function gaussian_elimination($coefficients, $vector) {
  $rows = count($coefficients);
  $columns = count($coefficients[0]);
  if ($rows != $columns) {
  return [];
}
  $augmented = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $columns) {
  $row = _append($row, $coefficients[$i][$j]);
  $j = $j + 1;
};
  $row = _append($row, $vector[$i][0]);
  $augmented = _append($augmented, $row);
  $i = $i + 1;
};
  $row_idx = 0;
  while ($row_idx < $rows - 1) {
  $pivot = $augmented[$row_idx][$row_idx];
  $col = $row_idx + 1;
  while ($col < $rows) {
  $factor = $augmented[$col][$row_idx] / $pivot;
  $k = $row_idx;
  while ($k < $columns + 1) {
  $augmented[$col][$k] = $augmented[$col][$k] - $factor * $augmented[$row_idx][$k];
  $k = $k + 1;
};
  $col = $col + 1;
};
  $row_idx = $row_idx + 1;
};
  $coeffs = [];
  $vec = [];
  $r = 0;
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $columns) {
  $row = _append($row, $augmented[$r][$c]);
  $c = $c + 1;
};
  $coeffs = _append($coeffs, $row);
  $vec = _append($vec, [$augmented[$r][$columns]]);
  $r = $r + 1;
};
  $x = retroactive_resolution($coeffs, $vec);
  return $x;
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(gaussian_elimination([[1.0, -4.0, -2.0], [5.0, 2.0, -2.0], [1.0, -1.0, 0.0]], [[-2.0], [-3.0], [4.0]]), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(gaussian_elimination([[1.0, 2.0], [5.0, 2.0]], [[5.0], [5.0]]), 1344)))))), PHP_EOL;
