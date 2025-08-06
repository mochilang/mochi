<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function sort_by_ratio(&$index, $ratio) {
  global $v, $w;
  $i = 1;
  while ($i < count($index)) {
  $key = $index[$i];
  $key_ratio = $ratio[$key];
  $j = $i - 1;
  while ($j >= 0 && $ratio[$index[$j]] < $key_ratio) {
  $index[$j + 1] = $index[$j];
  $j = $j - 1;
};
  $index[$j + 1] = $key;
  $i = $i + 1;
};
  return $index;
}
function fractional_knapsack($value, $weight, $capacity) {
  global $v, $w;
  $n = count($value);
  $index = [];
  $i = 0;
  while ($i < $n) {
  $index = _append($index, $i);
  $i = $i + 1;
};
  $ratio = [];
  $i = 0;
  while ($i < $n) {
  $ratio = _append($ratio, $value[$i] / $weight[$i]);
  $i = $i + 1;
};
  $index = sort_by_ratio($index, $ratio);
  $fractions = [];
  $i = 0;
  while ($i < $n) {
  $fractions = _append($fractions, 0.0);
  $i = $i + 1;
};
  $max_value = 0.0;
  $idx = 0;
  while ($idx < count($index)) {
  $item = $index[$idx];
  if ($weight[$item] <= $capacity) {
  $fractions[$item] = 1.0;
  $max_value = $max_value + $value[$item];
  $capacity = $capacity - $weight[$item];
} else {
  $fractions[$item] = $capacity / $weight[$item];
  $max_value = $max_value + $value[$item] * $capacity / $weight[$item];
  break;
}
  $idx = $idx + 1;
};
  return ['max_value' => $max_value, 'fractions' => $fractions];
}
$v = [1.0, 3.0, 5.0, 7.0, 9.0];
$w = [0.9, 0.7, 0.5, 0.3, 0.1];
echo rtrim(json_encode(fractional_knapsack($v, $w, 5.0), 1344)), PHP_EOL;
echo rtrim(json_encode(fractional_knapsack([1.0, 3.0, 5.0, 7.0], [0.9, 0.7, 0.5, 0.3], 30.0), 1344)), PHP_EOL;
echo rtrim(json_encode(fractional_knapsack([], [], 30.0), 1344)), PHP_EOL;
