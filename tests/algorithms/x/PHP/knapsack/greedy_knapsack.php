<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function calc_profit($profit, $weight, $max_weight) {
  if (count($profit) != count($weight)) {
  $panic('The length of profit and weight must be same.');
}
  if ($max_weight <= 0) {
  $panic('max_weight must greater than zero.');
}
  $i = 0;
  while ($i < count($profit)) {
  if ($profit[$i] < 0) {
  $panic('Profit can not be negative.');
}
  if ($weight[$i] < 0) {
  $panic('Weight can not be negative.');
}
  $i = $i + 1;
};
  $n = count($profit);
  $used = [];
  $j = 0;
  while ($j < $n) {
  $used = _append($used, false);
  $j = $j + 1;
};
  $limit = 0;
  $gain = 0.0;
  $count = 0;
  while ($limit < $max_weight && $count < $n) {
  $maxRatio = -1.0;
  $maxIndex = -1;
  $k = 0;
  while ($k < $n) {
  if (!$used[$k]) {
  $ratio = (floatval($profit[$k])) / (floatval($weight[$k]));
  if ($ratio > $maxRatio) {
  $maxRatio = $ratio;
  $maxIndex = $k;
};
}
  $k = $k + 1;
};
  if ($maxIndex < 0) {
  break;
}
  $used[$maxIndex] = true;
  if ($max_weight - $limit >= $weight[$maxIndex]) {
  $limit = $limit + $weight[$maxIndex];
  $gain = $gain + (floatval($profit[$maxIndex]));
} else {
  $gain = $gain + (floatval(($max_weight - $limit)) / (floatval($weight[$maxIndex]))) * (floatval($profit[$maxIndex]));
  break;
}
  $count = $count + 1;
};
  return $gain;
}
function main() {
  echo rtrim(json_encode(calc_profit([1, 2, 3], [3, 4, 5], 15), 1344)), PHP_EOL;
  echo rtrim(json_encode(calc_profit([10, 9, 8], [3, 4, 5], 25), 1344)), PHP_EOL;
  echo rtrim(json_encode(calc_profit([10, 9, 8], [3, 4, 5], 5), 1344)), PHP_EOL;
}
main();
