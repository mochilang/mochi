<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$seed = 1;
function mochi_rand() {
  global $seed, $pts;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
}
function random() {
  global $seed, $pts;
  return (floatval(mochi_rand())) / 2147483648.0;
}
function hypercube_points($num_points, $hypercube_size, $num_dimensions) {
  global $seed, $pts;
  $points = [];
  $i = 0;
  while ($i < $num_points) {
  $point = [];
  $j = 0;
  while ($j < $num_dimensions) {
  $value = $hypercube_size * random();
  $point = _append($point, $value);
  $j = $j + 1;
};
  $points = _append($points, $point);
  $i = $i + 1;
};
  return $points;
}
$pts = hypercube_points(3, 1.0, 2);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($pts, 1344))))))), PHP_EOL;
