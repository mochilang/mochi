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
$__start_mem = memory_get_usage();
$__start = _now();
  function parse_int($s) {
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = $value * 10 + (intval($c));
  $i = $i + 1;
};
  return $value;
};
  function is_polish_national_id($id) {
  if (strlen($id) == 0) {
  return false;
}
  if (substr($id, 0, 1) == '-') {
  return false;
}
  $input_int = parse_int($id);
  if ($input_int < 10100000 || $input_int > 99923199999) {
  return false;
}
  $month = parse_int(substr($id, 2, 4 - 2));
  if (!(($month >= 1 && $month <= 12) || ($month >= 21 && $month <= 32) || ($month >= 41 && $month <= 52) || ($month >= 61 && $month <= 72) || ($month >= 81 && $month <= 92))) {
  return false;
}
  $day = parse_int(substr($id, 4, 6 - 4));
  if ($day < 1 || $day > 31) {
  return false;
}
  $multipliers = [1, 3, 7, 9, 1, 3, 7, 9, 1, 3];
  $subtotal = 0;
  $i = 0;
  while ($i < count($multipliers)) {
  $digit = parse_int(substr($id, $i, $i + 1 - $i));
  $subtotal = $subtotal + fmod(($digit * $multipliers[$i]), 10);
  $i = $i + 1;
};
  $checksum = 10 - ($subtotal % 10);
  return $checksum == $input_int % 10;
};
  echo rtrim(_str(is_polish_national_id('02070803628'))), PHP_EOL;
  echo rtrim(_str(is_polish_national_id('02150803629'))), PHP_EOL;
  echo rtrim(_str(is_polish_national_id('02075503622'))), PHP_EOL;
  echo rtrim(_str(is_polish_national_id('-99012212349'))), PHP_EOL;
  echo rtrim(_str(is_polish_national_id('990122123499999'))), PHP_EOL;
  echo rtrim(_str(is_polish_national_id('02070803621'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
