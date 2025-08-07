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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function pow_int($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function armstrong_number($n) {
  if ($n < 1) {
  return false;
}
  $digits = 0;
  $temp = $n;
  while ($temp > 0) {
  $temp = _intdiv($temp, 10);
  $digits = $digits + 1;
};
  $total = 0;
  $temp = $n;
  while ($temp > 0) {
  $rem = $temp % 10;
  $total = $total + pow_int($rem, $digits);
  $temp = _intdiv($temp, 10);
};
  return $total == $n;
};
  function pluperfect_number($n) {
  if ($n < 1) {
  return false;
}
  $digit_histogram = [];
  $i = 0;
  while ($i < 10) {
  $digit_histogram = _append($digit_histogram, 0);
  $i = $i + 1;
};
  $digit_total = 0;
  $temp = $n;
  while ($temp > 0) {
  $rem = $temp % 10;
  $digit_histogram[$rem] = $digit_histogram[$rem] + 1;
  $digit_total = $digit_total + 1;
  $temp = _intdiv($temp, 10);
};
  $total = 0;
  $i = 0;
  while ($i < 10) {
  if ($digit_histogram[$i] > 0) {
  $total = $total + $digit_histogram[$i] * pow_int($i, $digit_total);
}
  $i = $i + 1;
};
  return $total == $n;
};
  function narcissistic_number($n) {
  if ($n < 1) {
  return false;
}
  $digits = 0;
  $temp = $n;
  while ($temp > 0) {
  $temp = _intdiv($temp, 10);
  $digits = $digits + 1;
};
  $temp = $n;
  $total = 0;
  while ($temp > 0) {
  $rem = $temp % 10;
  $total = $total + pow_int($rem, $digits);
  $temp = _intdiv($temp, 10);
};
  return $total == $n;
};
  echo rtrim(json_encode(armstrong_number(371), 1344)), PHP_EOL;
  echo rtrim(json_encode(armstrong_number(200), 1344)), PHP_EOL;
  echo rtrim(json_encode(pluperfect_number(371), 1344)), PHP_EOL;
  echo rtrim(json_encode(pluperfect_number(200), 1344)), PHP_EOL;
  echo rtrim(json_encode(narcissistic_number(371), 1344)), PHP_EOL;
  echo rtrim(json_encode(narcissistic_number(200), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
