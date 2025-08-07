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
  function pow10($n) {
  $p = 1.0;
  if ($n >= 0) {
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
} else {
  $i = 0;
  while ($i > $n) {
  $p = $p / 10.0;
  $i = $i - 1;
};
}
  return $p;
};
  function mochi_floor($x) {
  $i = intval($x);
  $f = floatval($i);
  if ($f > $x) {
  return floatval(($i - 1));
}
  return $f;
};
  function format_scientific_3($x) {
  if ($x == 0.0) {
  return '0.000e+00';
}
  $sign = '';
  $num = $x;
  if ($num < 0.0) {
  $sign = '-';
  $num = -$num;
}
  $exp = 0;
  while ($num >= 10.0) {
  $num = $num / 10.0;
  $exp = $exp + 1;
};
  while ($num < 1.0) {
  $num = $num * 10.0;
  $exp = $exp - 1;
};
  $temp = mochi_floor($num * 1000.0 + 0.5);
  $scaled = intval($temp);
  if ($scaled == 10000) {
  $scaled = 1000;
  $exp = $exp + 1;
}
  $int_part = _intdiv($scaled, 1000);
  $frac_part = $scaled % 1000;
  $frac_str = _str($frac_part);
  while (strlen($frac_str) < 3) {
  $frac_str = '0' . $frac_str;
};
  $mantissa = _str($int_part) . '.' . $frac_str;
  $exp_sign = '+';
  $exp_abs = $exp;
  if ($exp < 0) {
  $exp_sign = '-';
  $exp_abs = -$exp;
}
  $exp_str = _str($exp_abs);
  if ($exp_abs < 10) {
  $exp_str = '0' . $exp_str;
}
  return $sign . $mantissa . 'e' . $exp_sign . $exp_str;
};
  function orbital_transfer_work($mass_central, $mass_object, $r_initial, $r_final) {
  $G = 6.6743 * pow10(-11);
  if ($r_initial <= 0.0 || $r_final <= 0.0) {
  _panic('Orbital radii must be greater than zero.');
}
  $work = ($G * $mass_central * $mass_object / 2.0) * (1.0 / $r_initial - 1.0 / $r_final);
  return format_scientific_3($work);
};
  function test_orbital_transfer_work() {
  if (orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6)) != '2.811e+09') {
  _panic('case1 failed');
}
  if (orbital_transfer_work(5.972 * pow10(24), 500.0, 7.0 * pow10(6), 6.371 * pow10(6)) != '-1.405e+09') {
  _panic('case2 failed');
}
  if (orbital_transfer_work(1.989 * pow10(30), 1000.0, 1.5 * pow10(11), 2.28 * pow10(11)) != '1.514e+11') {
  _panic('case3 failed');
}
};
  function main() {
  test_orbital_transfer_work();
  echo rtrim(orbital_transfer_work(5.972 * pow10(24), 1000.0, 6.371 * pow10(6), 7.0 * pow10(6))), PHP_EOL;
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
