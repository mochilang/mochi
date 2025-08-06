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
$__start_mem = memory_get_usage();
$__start = _now();
  function panic($msg) {
  echo rtrim($msg), PHP_EOL;
};
  function trim_spaces($s) {
  $start = 0;
  $end = strlen($s) - 1;
  while ($start <= $end && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = $start + 1;
};
  while ($end >= $start && substr($s, $end, $end + 1 - $end) == ' ') {
  $end = $end - 1;
};
  if ($start > $end) {
  return '';
}
  return substr($s, $start, $end + 1 - $start);
};
  function char_to_digit($ch) {
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
  panic('Non-octal value was passed to the function');
  return 0;
};
  function oct_to_decimal($oct_string) {
  $s = trim_spaces($oct_string);
  if (strlen($s) == 0) {
  panic('Empty string was passed to the function');
  return 0;
}
  $is_negative = false;
  if (substr($s, 0, 1 - 0) == '-') {
  $is_negative = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  if (strlen($s) == 0) {
  panic('Non-octal value was passed to the function');
  return 0;
}
  $decimal_number = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  $digit = char_to_digit($ch);
  $decimal_number = 8 * $decimal_number + $digit;
  $i = $i + 1;
};
  if ($is_negative) {
  $decimal_number = -$decimal_number;
}
  return $decimal_number;
};
  function main() {
  echo rtrim(_str(oct_to_decimal('1'))), PHP_EOL;
  echo rtrim(_str(oct_to_decimal('-1'))), PHP_EOL;
  echo rtrim(_str(oct_to_decimal('12'))), PHP_EOL;
  echo rtrim(_str(oct_to_decimal(' 12   '))), PHP_EOL;
  echo rtrim(_str(oct_to_decimal('-45'))), PHP_EOL;
  echo rtrim(_str(oct_to_decimal('0'))), PHP_EOL;
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
