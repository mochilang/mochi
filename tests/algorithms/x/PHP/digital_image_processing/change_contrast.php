<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
$__start_mem = memory_get_usage();
$__start = _now();
  function change_contrast($img, $level) {
  global $image;
  $factor = (259.0 * ((floatval($level)) + 255.0)) / (255.0 * (259.0 - (floatval($level))));
  $result = [];
  $i = 0;
  while ($i < count($img)) {
  $row = $img[$i];
  $new_row = [];
  $j = 0;
  while ($j < count($row)) {
  $c = $row[$j];
  $contrasted = intval((128.0 + $factor * ((floatval($c)) - 128.0)));
  $clamped = ($contrasted < 0 ? 0 : ($contrasted > 255 ? 255 : $contrasted));
  $new_row = _append($new_row, $clamped);
  $j = $j + 1;
};
  $result = _append($result, $new_row);
  $i = $i + 1;
};
  return $result;
};
  function print_image($img) {
  global $contrasted, $image;
  $i = 0;
  while ($i < count($img)) {
  $row = $img[$i];
  $j = 0;
  $line = '';
  while ($j < count($row)) {
  $line = $line . _str($row[$j]) . ' ';
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  $image = [[100, 125, 150], [175, 200, 225], [50, 75, 100]];
  echo rtrim('Original image:'), PHP_EOL;
  print_image($image);
  $contrasted = change_contrast($image, 170);
  echo rtrim('After contrast:'), PHP_EOL;
  print_image($contrasted);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
