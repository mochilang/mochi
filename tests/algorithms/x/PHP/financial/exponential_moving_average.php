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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function exponential_moving_average($stock_prices, $window_size) {
  if ($window_size <= 0) {
  $panic('window_size must be > 0');
}
  $alpha = 2.0 / (1.0 + (floatval($window_size)));
  $moving_average = 0.0;
  $result = [];
  $i = 0;
  while ($i < count($stock_prices)) {
  $price = $stock_prices[$i];
  if ($i <= $window_size) {
  if ($i == 0) {
  $moving_average = $price;
} else {
  $moving_average = ($moving_average + $price) * 0.5;
};
} else {
  $moving_average = $alpha * $price + (1.0 - $alpha) * $moving_average;
}
  $result = _append($result, $moving_average);
  $i = $i + 1;
};
  return $result;
}
$stock_prices = [2.0, 5.0, 3.0, 8.2, 6.0, 9.0, 10.0];
$window_size = 3;
$result = exponential_moving_average($stock_prices, $window_size);
echo rtrim(_str($result)), PHP_EOL;
