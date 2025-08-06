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
  $SI_UNITS = ['yotta' => 24, 'zetta' => 21, 'exa' => 18, 'peta' => 15, 'tera' => 12, 'giga' => 9, 'mega' => 6, 'kilo' => 3, 'hecto' => 2, 'deca' => 1, 'deci' => -1, 'centi' => -2, 'milli' => -3, 'micro' => -6, 'nano' => -9, 'pico' => -12, 'femto' => -15, 'atto' => -18, 'zepto' => -21, 'yocto' => -24];
  $BINARY_UNITS = ['yotta' => 8, 'zetta' => 7, 'exa' => 6, 'peta' => 5, 'tera' => 4, 'giga' => 3, 'mega' => 2, 'kilo' => 1];
  function mochi_pow($base, $exp) {
  global $SI_UNITS, $BINARY_UNITS;
  if ($exp == 0) {
  return 1.0;
}
  $e = $exp;
  if ($e < 0) {
  $e = -$e;
}
  $result = 1.0;
  $i = 0;
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  if ($exp < 0) {
  return 1.0 / $result;
}
  return $result;
};
  function convert_si_prefix($known_amount, $known_prefix, $unknown_prefix) {
  global $SI_UNITS, $BINARY_UNITS;
  $kp = strtolower($known_prefix);
  $up = strtolower($unknown_prefix);
  if (!(array_key_exists($kp, $SI_UNITS))) {
  $panic('unknown prefix: ' . $known_prefix);
}
  if (!(array_key_exists($up, $SI_UNITS))) {
  $panic('unknown prefix: ' . $unknown_prefix);
}
  $diff = $SI_UNITS[$kp] - $SI_UNITS[$up];
  return $known_amount * mochi_pow(10.0, $diff);
};
  function convert_binary_prefix($known_amount, $known_prefix, $unknown_prefix) {
  global $SI_UNITS, $BINARY_UNITS;
  $kp = strtolower($known_prefix);
  $up = strtolower($unknown_prefix);
  if (!(array_key_exists($kp, $BINARY_UNITS))) {
  $panic('unknown prefix: ' . $known_prefix);
}
  if (!(array_key_exists($up, $BINARY_UNITS))) {
  $panic('unknown prefix: ' . $unknown_prefix);
}
  $diff = ($BINARY_UNITS[$kp] - $BINARY_UNITS[$up]) * 10;
  return $known_amount * mochi_pow(2.0, $diff);
};
  echo rtrim(_str(convert_si_prefix(1.0, 'giga', 'mega'))), PHP_EOL;
  echo rtrim(_str(convert_si_prefix(1.0, 'mega', 'giga'))), PHP_EOL;
  echo rtrim(_str(convert_si_prefix(1.0, 'kilo', 'kilo'))), PHP_EOL;
  echo rtrim(_str(convert_binary_prefix(1.0, 'giga', 'mega'))), PHP_EOL;
  echo rtrim(_str(convert_binary_prefix(1.0, 'mega', 'giga'))), PHP_EOL;
  echo rtrim(_str(convert_binary_prefix(1.0, 'kilo', 'kilo'))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
