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
  function panic($msg) {
  echo rtrim($msg), PHP_EOL;
};
  function trim_spaces($s) {
  $start = 0;
  $end = strlen($s);
  while ($start < $end && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end > $start && substr($s, $end - 1, $end - ($end - 1)) == ' ') {
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function hex_digit_value($ch) {
  if ($ch == '0') {
  return 0;
}
  if ($ch == '1') {
  return 1;
}
  if ($ch == '2') {
  return 2;
}
  if ($ch == '3') {
  return 3;
}
  if ($ch == '4') {
  return 4;
}
  if ($ch == '5') {
  return 5;
}
  if ($ch == '6') {
  return 6;
}
  if ($ch == '7') {
  return 7;
}
  if ($ch == '8') {
  return 8;
}
  if ($ch == '9') {
  return 9;
}
  if ($ch == 'a' || $ch == 'A') {
  return 10;
}
  if ($ch == 'b' || $ch == 'B') {
  return 11;
}
  if ($ch == 'c' || $ch == 'C') {
  return 12;
}
  if ($ch == 'd' || $ch == 'D') {
  return 13;
}
  if ($ch == 'e' || $ch == 'E') {
  return 14;
}
  if ($ch == 'f' || $ch == 'F') {
  return 15;
}
  panic('Invalid value was passed to the function');
};
  function hex_to_bin($hex_num) {
  $trimmed = trim_spaces($hex_num);
  if (strlen($trimmed) == 0) {
  panic('No value was passed to the function');
}
  $s = $trimmed;
  $is_negative = false;
  if (substr($s, 0, 1 - 0) == '-') {
  $is_negative = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $int_num = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $val = hex_digit_value($ch);
  $int_num = $int_num * 16 + $val;
  $i = $i + 1;
};
  $bin_str = '';
  $n = $int_num;
  if ($n == 0) {
  $bin_str = '0';
}
  while ($n > 0) {
  $bin_str = _str($n % 2) . $bin_str;
  $n = _intdiv($n, 2);
};
  $result = intval($bin_str);
  if ($is_negative) {
  $result = -$result;
}
  return $result;
};
  echo rtrim(_str(hex_to_bin('AC'))), PHP_EOL;
  echo rtrim(_str(hex_to_bin('9A4'))), PHP_EOL;
  echo rtrim(_str(hex_to_bin('   12f   '))), PHP_EOL;
  echo rtrim(_str(hex_to_bin('FfFf'))), PHP_EOL;
  echo rtrim(_str(hex_to_bin('-fFfF'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
