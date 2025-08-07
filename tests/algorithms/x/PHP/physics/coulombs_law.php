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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function format2($x) {
  global $K;
  $sign = ($x < 0.0 ? '-' : '');
  $y = ($x < 0.0 ? -$x : $x);
  $m = 100.0;
  $scaled = $y * $m;
  $i = intval($scaled);
  if ($scaled - (floatval($i)) >= 0.5) {
  $i = $i + 1;
}
  $int_part = _intdiv($i, 100);
  $frac_part = $i % 100;
  $frac_str = _str($frac_part);
  if ($frac_part < 10) {
  $frac_str = '0' . $frac_str;
}
  return $sign . _str($int_part) . '.' . $frac_str;
};
  $K = 8987551792.3;
  function coulombs_law($q1, $q2, $radius) {
  global $K;
  if ($radius <= 0.0) {
  _panic('radius must be positive');
}
  $force = $K * $q1 * $q2 / ($radius * $radius);
  return $force;
};
  echo rtrim(format2(coulombs_law(15.5, 20.0, 15.0))), PHP_EOL;
  echo rtrim(format2(coulombs_law(1.0, 15.0, 5.0))), PHP_EOL;
  echo rtrim(format2(coulombs_law(20.0, -50.0, 15.0))), PHP_EOL;
  echo rtrim(format2(coulombs_law(-5.0, -8.0, 10.0))), PHP_EOL;
  echo rtrim(format2(coulombs_law(50.0, 100.0, 50.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
