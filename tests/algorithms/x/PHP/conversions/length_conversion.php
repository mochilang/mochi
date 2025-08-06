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
$__start_mem = memory_get_usage();
$__start = _now();
  function rstrip_s($s) {
  if (strlen($s) > 0 && substr($s, strlen($s) - 1, strlen($s) - 1 + 1 - (strlen($s) - 1)) == 's') {
  return substr($s, 0, strlen($s) - 1 - 0);
}
  return $s;
};
  function normalize_alias($u) {
  if ($u == 'millimeter') {
  return 'mm';
}
  if ($u == 'centimeter') {
  return 'cm';
}
  if ($u == 'meter') {
  return 'm';
}
  if ($u == 'kilometer') {
  return 'km';
}
  if ($u == 'inch') {
  return 'in';
}
  if ($u == 'inche') {
  return 'in';
}
  if ($u == 'feet') {
  return 'ft';
}
  if ($u == 'foot') {
  return 'ft';
}
  if ($u == 'yard') {
  return 'yd';
}
  if ($u == 'mile') {
  return 'mi';
}
  return $u;
};
  function has_unit($u) {
  return $u == 'mm' || $u == 'cm' || $u == 'm' || $u == 'km' || $u == 'in' || $u == 'ft' || $u == 'yd' || $u == 'mi';
};
  function from_factor($u) {
  if ($u == 'mm') {
  return 0.001;
}
  if ($u == 'cm') {
  return 0.01;
}
  if ($u == 'm') {
  return 1.0;
}
  if ($u == 'km') {
  return 1000.0;
}
  if ($u == 'in') {
  return 0.0254;
}
  if ($u == 'ft') {
  return 0.3048;
}
  if ($u == 'yd') {
  return 0.9144;
}
  if ($u == 'mi') {
  return 1609.34;
}
  return 0.0;
};
  function to_factor($u) {
  if ($u == 'mm') {
  return 1000.0;
}
  if ($u == 'cm') {
  return 100.0;
}
  if ($u == 'm') {
  return 1.0;
}
  if ($u == 'km') {
  return 0.001;
}
  if ($u == 'in') {
  return 39.3701;
}
  if ($u == 'ft') {
  return 3.28084;
}
  if ($u == 'yd') {
  return 1.09361;
}
  if ($u == 'mi') {
  return 0.000621371;
}
  return 0.0;
};
  function length_conversion($value, $from_type, $to_type) {
  $new_from = normalize_alias(rstrip_s(strtolower($from_type)));
  $new_to = normalize_alias(rstrip_s(strtolower($to_type)));
  if (!has_unit($new_from)) {
  $panic('Invalid \'from_type\' value: \'' . $from_type . '\'.
Conversion abbreviations are: mm, cm, m, km, in, ft, yd, mi');
}
  if (!has_unit($new_to)) {
  $panic('Invalid \'to_type\' value: \'' . $to_type . '\'.
Conversion abbreviations are: mm, cm, m, km, in, ft, yd, mi');
}
  return $value * from_factor($new_from) * to_factor($new_to);
};
  echo rtrim(json_encode(length_conversion(4.0, 'METER', 'FEET'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(1.0, 'kilometer', 'inch'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(2.0, 'feet', 'meter'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(2.0, 'centimeter', 'millimeter'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(4.0, 'yard', 'kilometer'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(3.0, 'foot', 'inch'), 1344)), PHP_EOL;
  echo rtrim(json_encode(length_conversion(3.0, 'mm', 'in'), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
