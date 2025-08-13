<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function exponential_moving_average($stock_prices, $window_size) {
  global $result;
  if ($window_size <= 0) {
  _panic('window_size must be > 0');
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
};
  $stock_prices = [2.0, 5.0, 3.0, 8.2, 6.0, 9.0, 10.0];
  $window_size = 3;
  $result = exponential_moving_average($stock_prices, $window_size);
  echo rtrim(_str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
