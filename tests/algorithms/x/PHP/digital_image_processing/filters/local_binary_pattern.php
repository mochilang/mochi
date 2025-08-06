<?php
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function get_neighbors_pixel($image, $x, $y, $center) {
  global $i, $j, $line, $value;
  if ($x < 0 || $y < 0) {
  return 0;
}
  if ($x >= count($image) || $y >= count($image[0])) {
  return 0;
}
  if ($image[$x][$y] >= $center) {
  return 1;
}
  return 0;
}
function local_binary_value($image, $x, $y) {
  global $j, $line, $value;
  $center = $image[$x][$y];
  $powers = [1, 2, 4, 8, 16, 32, 64, 128];
  $neighbors = [get_neighbors_pixel($image, $x - 1, $y + 1, $center), get_neighbors_pixel($image, $x, $y + 1, $center), get_neighbors_pixel($image, $x - 1, $y, $center), get_neighbors_pixel($image, $x + 1, $y + 1, $center), get_neighbors_pixel($image, $x + 1, $y, $center), get_neighbors_pixel($image, $x + 1, $y - 1, $center), get_neighbors_pixel($image, $x, $y - 1, $center), get_neighbors_pixel($image, $x - 1, $y - 1, $center)];
  $sum = 0;
  $i = 0;
  while ($i < count($neighbors)) {
  $sum = $sum + $neighbors[$i] * $powers[$i];
  $i = $i + 1;
};
  return $sum;
}
$image = [[10, 10, 10, 10, 10], [10, 20, 30, 20, 10], [10, 30, 40, 30, 10], [10, 20, 30, 20, 10], [10, 10, 10, 10, 10]];
$i = 0;
while ($i < count($image)) {
  $j = 0;
  $line = '';
  while ($j < count($image[0])) {
  $value = local_binary_value($image, $i, $j);
  $line = $line . _str($value);
  if ($j < count($image[0]) - 1) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
}
