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
  $UNIT_SYMBOL = ['meter' => 'm', 'kilometer' => 'km', 'megametre' => 'Mm', 'gigametre' => 'Gm', 'terametre' => 'Tm', 'petametre' => 'Pm', 'exametre' => 'Em', 'zettametre' => 'Zm', 'yottametre' => 'Ym'];
  $METRIC_CONVERSION = ['m' => 0, 'km' => 3, 'Mm' => 6, 'Gm' => 9, 'Tm' => 12, 'Pm' => 15, 'Em' => 18, 'Zm' => 21, 'Ym' => 24];
  $ABBREVIATIONS = 'm, km, Mm, Gm, Tm, Pm, Em, Zm, Ym';
  function sanitize($unit) {
  global $UNIT_SYMBOL, $METRIC_CONVERSION, $ABBREVIATIONS;
  $res = strtolower($unit);
  if (strlen($res) > 0) {
  $last = substr($res, strlen($res) - 1, strlen($res) - (strlen($res) - 1));
  if ($last == 's') {
  $res = substr($res, 0, strlen($res) - 1 - 0);
};
}
  if (array_key_exists($res, $UNIT_SYMBOL)) {
  return $UNIT_SYMBOL[$res];
}
  return $res;
};
  function pow10($exp) {
  global $UNIT_SYMBOL, $METRIC_CONVERSION, $ABBREVIATIONS;
  if ($exp == 0) {
  return 1.0;
}
  $e = $exp;
  $res = 1.0;
  if ($e < 0) {
  $e = -$e;
}
  $i = 0;
  while ($i < $e) {
  $res = $res * 10.0;
  $i = $i + 1;
};
  if ($exp < 0) {
  return 1.0 / $res;
}
  return $res;
};
  function length_conversion($value, $from_type, $to_type) {
  global $UNIT_SYMBOL, $METRIC_CONVERSION, $ABBREVIATIONS;
  $from_sanitized = sanitize($from_type);
  $to_sanitized = sanitize($to_type);
  if (!(array_key_exists($from_sanitized, $METRIC_CONVERSION))) {
  $panic('Invalid \'from_type\' value: \'' . $from_type . '\'.
Conversion abbreviations are: ' . $ABBREVIATIONS);
}
  if (!(array_key_exists($to_sanitized, $METRIC_CONVERSION))) {
  $panic('Invalid \'to_type\' value: \'' . $to_type . '\'.
Conversion abbreviations are: ' . $ABBREVIATIONS);
}
  $from_exp = $METRIC_CONVERSION[$from_sanitized];
  $to_exp = $METRIC_CONVERSION[$to_sanitized];
  $exponent = 0;
  if ($from_exp > $to_exp) {
  $exponent = $from_exp - $to_exp;
} else {
  $exponent = -($to_exp - $from_exp);
}
  return $value * pow10($exponent);
};
  echo rtrim(_str(length_conversion(1.0, 'meter', 'kilometer'))), PHP_EOL;
  echo rtrim(_str(length_conversion(1.0, 'meter', 'megametre'))), PHP_EOL;
  echo rtrim(_str(length_conversion(1.0, 'gigametre', 'meter'))), PHP_EOL;
  echo rtrim(_str(length_conversion(1.0, 'terametre', 'zettametre'))), PHP_EOL;
  echo rtrim(_str(length_conversion(1.0, 'yottametre', 'zettametre'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
