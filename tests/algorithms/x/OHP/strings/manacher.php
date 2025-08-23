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
  function palindromic_string($input_string) {
  $max_length = 0;
  $new_input_string = '';
  $output_string = '';
  $n = strlen($input_string);
  $i = 0;
  while ($i < $n - 1) {
  $new_input_string = $new_input_string . substr($input_string, $i, $i + 1 - $i) . '|';
  $i = $i + 1;
};
  $new_input_string = $new_input_string . substr($input_string, $n - 1, $n - ($n - 1));
  $left = 0;
  $right = 0;
  $length = [];
  $i = 0;
  $m = strlen($new_input_string);
  while ($i < $m) {
  $length = _append($length, 1);
  $i = $i + 1;
};
  $start = 0;
  $j = 0;
  while ($j < $m) {
  $k = 1;
  if ($j <= $right) {
  $mirror = $left + $right - $j;
  $k = $length[$mirror] / 2;
  $diff = $right - $j + 1;
  if ($diff < $k) {
  $k = $diff;
};
  if ($k < 1) {
  $k = 1;
};
}
  while ($j - $k >= 0 && $j + $k < $m && substr($new_input_string, $j + $k, $j + $k + 1 - ($j + $k)) == substr($new_input_string, $j - $k, $j - $k + 1 - ($j - $k))) {
  $k = $k + 1;
};
  $length[$j] = 2 * $k - 1;
  if ($j + $k - 1 > $right) {
  $left = $j - $k + 1;
  $right = $j + $k - 1;
}
  if ($length[$j] > $max_length) {
  $max_length = $length[$j];
  $start = $j;
}
  $j = $j + 1;
};
  $s = substr($new_input_string, $start - _intdiv($max_length, 2), $start + _intdiv($max_length, 2) + 1 - ($start - _intdiv($max_length, 2)));
  $idx = 0;
  while ($idx < strlen($s)) {
  $ch = substr($s, $idx, $idx + 1 - $idx);
  if ($ch != '|') {
  $output_string = $output_string . $ch;
}
  $idx = $idx + 1;
};
  return $output_string;
};
  function main() {
  echo rtrim(palindromic_string('abbbaba')), PHP_EOL;
  echo rtrim(palindromic_string('ababa')), PHP_EOL;
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
