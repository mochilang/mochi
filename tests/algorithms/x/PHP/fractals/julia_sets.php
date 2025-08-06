<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function complex_add($a, $b) {
  return ['re' => $a['re'] + $b['re'], 'im' => $a['im'] + $b['im']];
}
function complex_mul($a, $b) {
  $real = $a['re'] * $b['re'] - $a['im'] * $b['im'];
  $imag = $a['re'] * $b['im'] + $a['im'] * $b['re'];
  return ['re' => $real, 'im' => $imag];
}
function sqrtApprox($x) {
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function complex_abs($a) {
  return sqrtApprox($a['re'] * $a['re'] + $a['im'] * $a['im']);
}
function sin_taylor($x) {
  $term = $x;
  $sum = $x;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i));
  $k2 = 2.0 * (floatval($i)) + 1.0;
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
}
function cos_taylor($x) {
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 10) {
  $k1 = 2.0 * (floatval($i)) - 1.0;
  $k2 = 2.0 * (floatval($i));
  $term = -$term * $x * $x / ($k1 * $k2);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
}
function exp_taylor($x) {
  $term = 1.0;
  $sum = 1.0;
  $i = 1.0;
  while ($i < 20.0) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1.0;
};
  return $sum;
}
function complex_exp($z) {
  $e = exp_taylor($z['re']);
  return ['re' => $e * cos_taylor($z['im']), 'im' => $e * sin_taylor($z['im'])];
}
function eval_quadratic($c, $z) {
  return complex_add(complex_mul($z, $z), $c);
}
function eval_exponential($c, $z) {
  return complex_add(complex_exp($z), $c);
}
function iterate_function($eval_function, $c, $nb_iterations, $z0, $infinity) {
  $z_n = $z0;
  $i = 0;
  while ($i < $nb_iterations) {
  $z_n = $eval_function($c, $z_n);
  if (complex_abs($z_n) > $infinity) {
  return $z_n;
}
  $i = $i + 1;
};
  return $z_n;
}
function prepare_grid($window_size, $nb_pixels) {
  $grid = [];
  $i = 0;
  while ($i < $nb_pixels) {
  $row = [];
  $j = 0;
  while ($j < $nb_pixels) {
  $real = -$window_size + 2.0 * $window_size * (floatval($i)) / (floatval(($nb_pixels - 1)));
  $imag = -$window_size + 2.0 * $window_size * (floatval($j)) / (floatval(($nb_pixels - 1)));
  $row = _append($row, ['re' => $real, 'im' => $imag]);
  $j = $j + 1;
};
  $grid = _append($grid, $row);
  $i = $i + 1;
};
  return $grid;
}
function julia_demo() {
  $grid = prepare_grid(1.0, 5);
  $c_poly = ['re' => -0.4, 'im' => 0.6];
  $c_exp = ['re' => -2.0, 'im' => 0.0];
  $poly_result = [];
  $exp_result = [];
  $y = 0;
  while ($y < count($grid)) {
  $row_poly = [];
  $row_exp = [];
  $x = 0;
  while ($x < count($grid[$y])) {
  $z0 = $grid[$y][$x];
  $z_poly = iterate_function('eval_quadratic', $c_poly, 20, $z0, 4.0);
  $z_exp = iterate_function('eval_exponential', $c_exp, 10, $z0, 10000000000.0);
  $row_poly = _append($row_poly, (complex_abs($z_poly) < 2.0 ? 1 : 0));
  $row_exp = _append($row_exp, (complex_abs($z_exp) < 10000.0 ? 1 : 0));
  $x = $x + 1;
};
  $poly_result = _append($poly_result, $row_poly);
  $exp_result = _append($exp_result, $row_exp);
  $y = $y + 1;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($poly_result, 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($exp_result, 1344)))))), PHP_EOL;
}
julia_demo();
