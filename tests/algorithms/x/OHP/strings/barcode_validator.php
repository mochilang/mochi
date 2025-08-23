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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function has_alpha($s) {
  global $res, $x;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if (($c >= 'a' && $c <= 'z') || ($c >= 'A' && $c <= 'Z')) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function parse_decimal($s) {
  global $res, $x;
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  _panic('Non-digit character encountered');
}
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
};
  function get_barcode($barcode) {
  global $res, $x;
  if (has_alpha($barcode)) {
  _panic('Barcode \'' . $barcode . '\' has alphabetic characters.');
}
  if (strlen($barcode) > 0 && substr($barcode, 0, 0 + 1) == '-') {
  _panic('The entered barcode has a negative value. Try again.');
}
  return parse_decimal($barcode);
};
  function get_check_digit($barcode) {
  global $res, $x;
  $num = _intdiv($barcode, 10);
  $s = 0;
  $position = 0;
  while ($num != 0) {
  $mult = ($position % 2 == 0 ? 3 : 1);
  $s = $s + $mult * ($num % 10);
  $num = _intdiv($num, 10);
  $position = $position + 1;
};
  return (10 - ($s % 10)) % 10;
};
  function is_valid($barcode) {
  global $res, $x;
  return strlen(_str($barcode)) == 13 && get_check_digit($barcode) == $barcode % 10;
};
  echo rtrim(_str(get_check_digit(8718452538119))), PHP_EOL;
  echo rtrim(_str(get_check_digit(87184523))), PHP_EOL;
  echo rtrim(_str(get_check_digit(87193425381086))), PHP_EOL;
  $res = [];
  $x = 0;
  while ($x < 100) {
  $res = _append($res, get_check_digit($x));
  $x = $x + 10;
}
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($res, 1344)))))), PHP_EOL;
  echo rtrim(_str(is_valid(8718452538119))), PHP_EOL;
  echo rtrim(_str(is_valid(87184525))), PHP_EOL;
  echo rtrim(_str(is_valid(87193425381089))), PHP_EOL;
  echo rtrim(_str(is_valid(0))), PHP_EOL;
  echo rtrim(_str(get_barcode('8718452538119'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
