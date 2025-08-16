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
function _len($x) {
    if ($x === null) { return 0; }
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function ratio($item) {
  global $items1, $items2, $items3, $items4;
  return (floatval($item['value'])) / (floatval($item['weight']));
};
  function fractional_cover($items, $capacity) {
  global $items1, $items2, $items3, $items4;
  if ($capacity < 0) {
  _panic('Capacity cannot be negative');
}
  $total = 0.0;
  $remaining = $capacity;
  $sorted = [];
foreach ($items as $it) {
  $sorted[] = $it;
}
;
  $idx = 0;
  while ($idx < _len($sorted) && $remaining > 0) {
  $item = $sorted[$idx];
  $take = ($item['weight'] < $remaining ? $item['weight'] : $remaining);
  $total = $total + (floatval($take)) * ratio($item);
  $remaining = $remaining - $take;
  $idx = $idx + 1;
};
  return $total;
};
  $items1 = [['weight' => 10, 'value' => 60], ['weight' => 20, 'value' => 100], ['weight' => 30, 'value' => 120]];
  echo rtrim(_str(fractional_cover($items1, 50))), PHP_EOL;
  $items2 = [['weight' => 20, 'value' => 100], ['weight' => 30, 'value' => 120], ['weight' => 10, 'value' => 60]];
  echo rtrim(_str(fractional_cover($items2, 25))), PHP_EOL;
  $items3 = [];
  echo rtrim(_str(fractional_cover($items3, 50))), PHP_EOL;
  $items4 = [['weight' => 10, 'value' => 60]];
  echo rtrim(_str(fractional_cover($items4, 5))), PHP_EOL;
  echo rtrim(_str(fractional_cover($items4, 1))), PHP_EOL;
  echo rtrim(_str(fractional_cover($items4, 0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
