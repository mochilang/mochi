<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
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
}
function zeller_day($date_input) {
  $days = [0 => 'Sunday', 1 => 'Monday', 2 => 'Tuesday', 3 => 'Wednesday', 4 => 'Thursday', 5 => 'Friday', 6 => 'Saturday'];
  if (strlen($date_input) != 10) {
  _panic('Must be 10 characters long');
}
  $m = parse_decimal(substr($date_input, 0, 2));
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
}
function zeller($date_input) {
  $day = zeller_day($date_input);
  return 'Your date ' . $date_input . ', is a ' . $day . '!';
}
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
}
function main() {
  test_zeller();
  echo rtrim(zeller('01-31-2010')), PHP_EOL;
}
main();
