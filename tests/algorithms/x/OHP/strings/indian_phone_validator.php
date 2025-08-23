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
  function all_digits($s) {
  if (strlen($s) == 0) {
  return false;
}
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function indian_phone_validator($phone) {
  $s = $phone;
  if (strlen($s) >= 3 && substr($s, 0, 3) == '+91') {
  $s = substr($s, 3, strlen($s) - 3);
  if (strlen($s) > 0) {
  $c = substr($s, 0, 0 + 1);
  if ($c == '-' || $c == ' ') {
  $s = substr($s, 1, strlen($s) - 1);
};
};
}
  if (strlen($s) > 0 && substr($s, 0, 0 + 1) == '0') {
  $s = substr($s, 1, strlen($s) - 1);
}
  if (strlen($s) >= 2 && substr($s, 0, 2) == '91') {
  $s = substr($s, 2, strlen($s) - 2);
}
  if (strlen($s) != 10) {
  return false;
}
  $first = substr($s, 0, 0 + 1);
  if (!($first == '7' || $first == '8' || $first == '9')) {
  return false;
}
  if (!all_digits($s)) {
  return false;
}
  return true;
};
  echo rtrim(_str(indian_phone_validator('+91123456789'))), PHP_EOL;
  echo rtrim(_str(indian_phone_validator('+919876543210'))), PHP_EOL;
  echo rtrim(_str(indian_phone_validator('01234567896'))), PHP_EOL;
  echo rtrim(_str(indian_phone_validator('919876543218'))), PHP_EOL;
  echo rtrim(_str(indian_phone_validator('+91-1234567899'))), PHP_EOL;
  echo rtrim(_str(indian_phone_validator('+91-9876543218'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
