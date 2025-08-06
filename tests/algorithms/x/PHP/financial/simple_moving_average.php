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
function simple_moving_average($data, $window_size) {
  global $sma_values, $idx, $item;
  if ($window_size < 1) {
  $panic('Window size must be a positive integer');
}
  $result = [];
  $window_sum = 0.0;
  $i = 0;
  while ($i < count($data)) {
  $window_sum = $window_sum + $data[$i];
  if ($i >= $window_size) {
  $window_sum = $window_sum - $data[$i - $window_size];
}
  if ($i >= $window_size - 1) {
  $avg = $window_sum / $window_size;
  $result = _append($result, ['value' => $avg, 'ok' => true]);
} else {
  $result = _append($result, ['value' => 0.0, 'ok' => false]);
}
  $i = $i + 1;
};
  return $result;
}
$data = [10.0, 12.0, 15.0, 13.0, 14.0, 16.0, 18.0, 17.0, 19.0, 21.0];
$window_size = 3;
$sma_values = simple_moving_average($data, $window_size);
$idx = 0;
while ($idx < count($sma_values)) {
  $item = $sma_values[$idx];
  if ($item['ok']) {
  echo rtrim('Day ' . _str($idx + 1) . ': ' . _str($item['value'])), PHP_EOL;
} else {
  echo rtrim('Day ' . _str($idx + 1) . ': Not enough data for SMA'), PHP_EOL;
}
  $idx = $idx + 1;
}
