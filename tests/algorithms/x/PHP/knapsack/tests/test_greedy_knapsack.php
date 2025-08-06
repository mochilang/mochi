<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function calc_profit($profit, $weight, $max_weight) {
  if (count($profit) != count($weight)) {
  return ['ok' => false, 'value' => 0.0, 'error' => 'The length of profit and weight must be same.'];
}
  if ($max_weight <= 0) {
  return ['ok' => false, 'value' => 0.0, 'error' => 'max_weight must greater than zero.'];
}
  $i = 0;
  while ($i < count($profit)) {
  if ($profit[$i] < 0) {
  return ['ok' => false, 'value' => 0.0, 'error' => 'Profit can not be negative.'];
}
  if ($weight[$i] < 0) {
  return ['ok' => false, 'value' => 0.0, 'error' => 'Weight can not be negative.'];
}
  $i = $i + 1;
};
  $used = [];
  $j = 0;
  while ($j < count($profit)) {
  $used = _append($used, false);
  $j = $j + 1;
};
  $limit = 0;
  $gain = 0.0;
  while ($limit < $max_weight) {
  $max_ratio = -1.0;
  $idx = 0 - 1;
  $k = 0;
  while ($k < count($profit)) {
  if (!$used[$k]) {
  $ratio = (floatval($profit[$k])) / (floatval($weight[$k]));
  if ($ratio > $max_ratio) {
  $max_ratio = $ratio;
  $idx = $k;
};
}
  $k = $k + 1;
};
  if ($idx == 0 - 1) {
  break;
}
  $used[$idx] = true;
  if ($max_weight - $limit >= $weight[$idx]) {
  $limit = $limit + $weight[$idx];
  $gain = $gain + (floatval($profit[$idx]));
} else {
  $gain = $gain + ((floatval(($max_weight - $limit))) / (floatval($weight[$idx]))) * (floatval($profit[$idx]));
  break;
}
};
  return ['ok' => true, 'value' => $gain, 'error' => ''];
}
function test_sorted() {
  $profit = [10, 20, 30, 40, 50, 60];
  $weight = [2, 4, 6, 8, 10, 12];
  $res = calc_profit($profit, $weight, 100);
  return $res['ok'] && $res['value'] == 210.0;
}
function test_negative_max_weight() {
  $profit = [10, 20, 30, 40, 50, 60];
  $weight = [2, 4, 6, 8, 10, 12];
  $res = calc_profit($profit, $weight, -15);
  return !$res['ok'] && $res['error'] == 'max_weight must greater than zero.';
}
function test_negative_profit_value() {
  $profit = [10, -20, 30, 40, 50, 60];
  $weight = [2, 4, 6, 8, 10, 12];
  $res = calc_profit($profit, $weight, 15);
  return !$res['ok'] && $res['error'] == 'Profit can not be negative.';
}
function test_negative_weight_value() {
  $profit = [10, 20, 30, 40, 50, 60];
  $weight = [2, -4, 6, -8, 10, 12];
  $res = calc_profit($profit, $weight, 15);
  return !$res['ok'] && $res['error'] == 'Weight can not be negative.';
}
function test_null_max_weight() {
  $profit = [10, 20, 30, 40, 50, 60];
  $weight = [2, 4, 6, 8, 10, 12];
  $res = calc_profit($profit, $weight, 0);
  return !$res['ok'] && $res['error'] == 'max_weight must greater than zero.';
}
function test_unequal_list_length() {
  $profit = [10, 20, 30, 40, 50];
  $weight = [2, 4, 6, 8, 10, 12];
  $res = calc_profit($profit, $weight, 100);
  return !$res['ok'] && $res['error'] == 'The length of profit and weight must be same.';
}
echo rtrim(json_encode(test_sorted(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_negative_max_weight(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_negative_profit_value(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_negative_weight_value(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_null_max_weight(), 1344)), PHP_EOL;
echo rtrim(json_encode(test_unequal_list_length(), 1344)), PHP_EOL;
echo rtrim((true ? 'true' : 'false')), PHP_EOL;
