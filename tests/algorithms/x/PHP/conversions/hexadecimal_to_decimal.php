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
  function strip($s) {
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
  function hex_digit_value($c) {
  if ($c == '0') {
  return 0;
}
  if ($c == '1') {
  return 1;
}
  if ($c == '2') {
  return 2;
}
  if ($c == '3') {
  return 3;
}
  if ($c == '4') {
  return 4;
}
  if ($c == '5') {
  return 5;
}
  if ($c == '6') {
  return 6;
}
  if ($c == '7') {
  return 7;
}
  if ($c == '8') {
  return 8;
}
  if ($c == '9') {
  return 9;
}
  if ($c == 'a' || $c == 'A') {
  return 10;
}
  if ($c == 'b' || $c == 'B') {
  return 11;
}
  if ($c == 'c' || $c == 'C') {
  return 12;
}
  if ($c == 'd' || $c == 'D') {
  return 13;
}
  if ($c == 'e' || $c == 'E') {
  return 14;
}
  if ($c == 'f' || $c == 'F') {
  return 15;
}
  echo rtrim('Non-hexadecimal value was passed to the function'), PHP_EOL;
  return 0;
};
  function hex_to_decimal($hex_string) {
  $s = strip($hex_string);
  if (strlen($s) == 0) {
  echo rtrim('Empty string was passed to the function'), PHP_EOL;
  return 0;
}
  $is_negative = false;
  if (substr($s, 0, 1 - 0) == '-') {
  $is_negative = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $decimal_number = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = hex_digit_value($c);
  $decimal_number = 16 * $decimal_number + $value;
  $i = $i + 1;
};
  if ($is_negative) {
  return -$decimal_number;
}
  return $decimal_number;
};
  function main() {
  echo rtrim(_str(hex_to_decimal('a'))), PHP_EOL;
  echo rtrim(_str(hex_to_decimal('12f'))), PHP_EOL;
  echo rtrim(_str(hex_to_decimal('   12f   '))), PHP_EOL;
  echo rtrim(_str(hex_to_decimal('FfFf'))), PHP_EOL;
  echo rtrim(_str(hex_to_decimal('-Ff'))), PHP_EOL;
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
