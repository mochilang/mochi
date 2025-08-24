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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_greyscale($blue, $green, $red) {
  $b = floatval($blue);
  $g = floatval($green);
  $r = floatval($red);
  return intval((0.114 * $b + 0.587 * $g + 0.299 * $r));
};
  function zeros($h, $w) {
  $table = [];
  $i = 0;
  while ($i < $h) {
  $row = [];
  $j = 0;
  while ($j < $w) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $table = _append($table, $row);
  $i = $i + 1;
};
  return $table;
};
  function burkes_dither($img, $threshold) {
  $height = count($img);
  $width = count($img[0]);
  $error_table = zeros($height + 1, $width + 4);
  $output = [];
  $y = 0;
  while ($y < $height) {
  $row = [];
  $x = 0;
  while ($x < $width) {
  $px = $img[$y][$x];
  $grey = get_greyscale($px[0], $px[1], $px[2]);
  $total = $grey + $error_table[$y][$x + 2];
  $new_val = 0;
  $current_error = 0;
  if ($threshold > $total) {
  $new_val = 0;
  $current_error = $total;
} else {
  $new_val = 255;
  $current_error = $total - 255;
}
  $row = _append($row, $new_val);
  $error_table[$y][$x + 3] = $error_table[$y][$x + 3] + _intdiv((8 * $current_error), 32);
  $error_table[$y][$x + 4] = $error_table[$y][$x + 4] + _intdiv((4 * $current_error), 32);
  $error_table[$y + 1][$x + 2] = $error_table[$y + 1][$x + 2] + _intdiv((8 * $current_error), 32);
  $error_table[$y + 1][$x + 3] = $error_table[$y + 1][$x + 3] + _intdiv((4 * $current_error), 32);
  $error_table[$y + 1][$x + 4] = $error_table[$y + 1][$x + 4] + _intdiv((2 * $current_error), 32);
  $error_table[$y + 1][$x + 1] = $error_table[$y + 1][$x + 1] + _intdiv((4 * $current_error), 32);
  $error_table[$y + 1][$x] = $error_table[$y + 1][$x] + _intdiv((2 * $current_error), 32);
  $x = $x + 1;
};
  $output = _append($output, $row);
  $y = $y + 1;
};
  return $output;
};
  function main() {
  $img = [[[0, 0, 0], [64, 64, 64], [128, 128, 128], [192, 192, 192]], [[255, 255, 255], [200, 200, 200], [150, 150, 150], [100, 100, 100]], [[30, 144, 255], [255, 0, 0], [0, 255, 0], [0, 0, 255]], [[50, 100, 150], [80, 160, 240], [70, 140, 210], [60, 120, 180]]];
  $result = burkes_dither($img, 128);
  $y = 0;
  while ($y < count($result)) {
  $line = '';
  $x = 0;
  while ($x < count($result[$y])) {
  $line = $line . _str($result[$y][$x]);
  if ($x < count($result[$y]) - 1) {
  $line = $line . ' ';
}
  $x = $x + 1;
};
  echo rtrim($line), PHP_EOL;
  $y = $y + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
