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
  function simple_moving_average($data, $window_size) {
  global $idx, $item, $sma_values;
  if ($window_size < 1) {
  _panic('Window size must be a positive integer');
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
};
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
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
