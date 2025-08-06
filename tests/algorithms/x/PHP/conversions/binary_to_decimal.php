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
  function mochi_trim($s) {
  $start = 0;
  while ($start < strlen($s)) {
  $ch = substr($s, $start, $start + 1 - $start);
  if ($ch != ' ' && $ch != '
' && $ch != '	' && $ch != '') {
  break;
}
  $start = $start + 1;
};
  $end = strlen($s);
  while ($end > $start) {
  $ch = substr($s, $end - 1, $end - ($end - 1));
  if ($ch != ' ' && $ch != '
' && $ch != '	' && $ch != '') {
  break;
}
  $end = $end - 1;
};
  return substr($s, $start, $end - $start);
};
  function bin_to_decimal($bin_string) {
  $trimmed = mochi_trim($bin_string);
  if ($trimmed == '') {
  $panic('Empty string was passed to the function');
}
  $is_negative = false;
  $s = $trimmed;
  if (substr($s, 0, 1 - 0) == '-') {
  $is_negative = true;
  $s = substr($s, 1, strlen($s) - 1);
}
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c != '0' && $c != '1') {
  $panic('Non-binary value was passed to the function');
}
  $i = $i + 1;
};
  $decimal_number = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $digit = intval($c);
  $decimal_number = 2 * $decimal_number + $digit;
  $i = $i + 1;
};
  if ($is_negative) {
  return -$decimal_number;
}
  return $decimal_number;
};
  echo rtrim(_str(bin_to_decimal('101'))), PHP_EOL;
  echo rtrim(_str(bin_to_decimal(' 1010   '))), PHP_EOL;
  echo rtrim(_str(bin_to_decimal('-11101'))), PHP_EOL;
  echo rtrim(_str(bin_to_decimal('0'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
