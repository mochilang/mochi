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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function parse_decimal($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  _panic('invalid literal');
}
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
};
  function zeller_day($date_input) {
  $days = [0 => 'Sunday', 1 => 'Monday', 2 => 'Tuesday', 3 => 'Wednesday', 4 => 'Thursday', 5 => 'Friday', 6 => 'Saturday'];
  if (strlen($date_input) != 10) {
  _panic('Must be 10 characters long');
}
  $m = parse_decimal(substr($date_input, 0, 2 - 0));
  if ($m <= 0 || $m >= 13) {
  _panic('Month must be between 1 - 12');
}
  $sep1 = substr($date_input, 2, 2 + 1 - 2);
  if ($sep1 != '-' && $sep1 != '/') {
  _panic('Date separator must be \'-\' or \'/\'');
}
  $d = parse_decimal(substr($date_input, 3, 5 - 3));
  if ($d <= 0 || $d >= 32) {
  _panic('Date must be between 1 - 31');
}
  $sep2 = substr($date_input, 5, 5 + 1 - 5);
  if ($sep2 != '-' && $sep2 != '/') {
  _panic('Date separator must be \'-\' or \'/\'');
}
  $y = parse_decimal(substr($date_input, 6, 10 - 6));
  if ($y <= 45 || $y >= 8500) {
  _panic('Year out of range. There has to be some sort of limit...right?');
}
  $year = $y;
  $month = $m;
  if ($month <= 2) {
  $year = $year - 1;
  $month = $month + 12;
}
  $c = _intdiv($year, 100);
  $k = $year % 100;
  $t = intval(2.6 * (floatval($month)) - 5.39);
  $u = _intdiv($c, 4);
  $v = _intdiv($k, 4);
  $x = $d + $k;
  $z = $t + $u + $v + $x;
  $w = $z - (2 * $c);
  $f = $w % 7;
  if ($f < 0) {
  $f = $f + 7;
}
  return $days[$f];
};
  function zeller($date_input) {
  $day = zeller_day($date_input);
  return 'Your date ' . $date_input . ', is a ' . $day . '!';
};
  function test_zeller() {
  $inputs = ['01-31-2010', '02-01-2010', '11-26-2024', '07-04-1776'];
  $expected = ['Sunday', 'Monday', 'Tuesday', 'Thursday'];
  $i = 0;
  while ($i < count($inputs)) {
  $res = zeller_day($inputs[$i]);
  if ($res != $expected[$i]) {
  _panic('zeller test failed');
}
  $i = $i + 1;
};
};
  function main() {
  test_zeller();
  echo rtrim(zeller('01-31-2010')), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
