<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function inverse_of_matrix($matrix) {
  global $m2, $m3;
  if (count($matrix) == 2 && count($matrix[0]) == 2 && count($matrix[1]) == 2) {
  $det = $matrix[0][0] * $matrix[1][1] - $matrix[1][0] * $matrix[0][1];
  if ($det == 0.0) {
  echo rtrim('This matrix has no inverse.'), PHP_EOL;
  return [];
};
  return [[$matrix[1][1] / $det, -$matrix[0][1] / $det], [-$matrix[1][0] / $det, $matrix[0][0] / $det]];
} else {
  if (count($matrix) == 3 && count($matrix[0]) == 3 && count($matrix[1]) == 3 && count($matrix[2]) == 3) {
  $det = $matrix[0][0] * $matrix[1][1] * $matrix[2][2] + $matrix[0][1] * $matrix[1][2] * $matrix[2][0] + $matrix[0][2] * $matrix[1][0] * $matrix[2][1] - ($matrix[0][2] * $matrix[1][1] * $matrix[2][0] + $matrix[0][1] * $matrix[1][0] * $matrix[2][2] + $matrix[0][0] * $matrix[1][2] * $matrix[2][1]);
  if ($det == 0.0) {
  echo rtrim('This matrix has no inverse.'), PHP_EOL;
  return [];
};
  $cof = [[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]];
  $cof[0][0] = $matrix[1][1] * $matrix[2][2] - $matrix[1][2] * $matrix[2][1];
  $cof[0][1] = -($matrix[1][0] * $matrix[2][2] - $matrix[1][2] * $matrix[2][0]);
  $cof[0][2] = $matrix[1][0] * $matrix[2][1] - $matrix[1][1] * $matrix[2][0];
  $cof[1][0] = -($matrix[0][1] * $matrix[2][2] - $matrix[0][2] * $matrix[2][1]);
  $cof[1][1] = $matrix[0][0] * $matrix[2][2] - $matrix[0][2] * $matrix[2][0];
  $cof[1][2] = -($matrix[0][0] * $matrix[2][1] - $matrix[0][1] * $matrix[2][0]);
  $cof[2][0] = $matrix[0][1] * $matrix[1][2] - $matrix[0][2] * $matrix[1][1];
  $cof[2][1] = -($matrix[0][0] * $matrix[1][2] - $matrix[0][2] * $matrix[1][0]);
  $cof[2][2] = $matrix[0][0] * $matrix[1][1] - $matrix[0][1] * $matrix[1][0];
  $inv = [[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]];
  $i = 0;
  while ($i < 3) {
  $j = 0;
  while ($j < 3) {
  $inv[$i][$j] = $cof[$j][$i] / $det;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $inv;
};
}
  echo rtrim('Please provide a matrix of size 2x2 or 3x3.'), PHP_EOL;
  return [];
}
$m2 = [[2.0, 5.0], [2.0, 0.0]];
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inverse_of_matrix($m2), 1344)))))), PHP_EOL;
$m3 = [[2.0, 5.0, 7.0], [2.0, 0.0, 1.0], [1.0, 2.0, 3.0]];
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inverse_of_matrix($m3), 1344)))))), PHP_EOL;
