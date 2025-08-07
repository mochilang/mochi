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
$__start_mem = memory_get_usage();
$__start = _now();
  function abs_val($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function extended_euclidean_algorithm($a, $b) {
  if (abs_val($a) == 1) {
  return ['x' => $a, 'y' => 0];
}
  if (abs_val($b) == 1) {
  return ['x' => 0, 'y' => $b];
}
  $old_remainder = $a;
  $remainder = $b;
  $old_coeff_a = 1;
  $coeff_a = 0;
  $old_coeff_b = 0;
  $coeff_b = 1;
  while ($remainder != 0) {
  $quotient = _intdiv($old_remainder, $remainder);
  $temp_remainder = $old_remainder - $quotient * $remainder;
  $old_remainder = $remainder;
  $remainder = $temp_remainder;
  $temp_a = $old_coeff_a - $quotient * $coeff_a;
  $old_coeff_a = $coeff_a;
  $coeff_a = $temp_a;
  $temp_b = $old_coeff_b - $quotient * $coeff_b;
  $old_coeff_b = $coeff_b;
  $coeff_b = $temp_b;
};
  if ($a < 0) {
  $old_coeff_a = -$old_coeff_a;
}
  if ($b < 0) {
  $old_coeff_b = -$old_coeff_b;
}
  return ['x' => $old_coeff_a, 'y' => $old_coeff_b];
};
  function test_extended_euclidean_algorithm() {
  $r1 = extended_euclidean_algorithm(1, 24);
  if (($r1['x'] != 1) || ($r1['y'] != 0)) {
  $panic('test1 failed');
}
  $r2 = extended_euclidean_algorithm(8, 14);
  if (($r2['x'] != 2) || ($r2['y'] != (-1))) {
  $panic('test2 failed');
}
  $r3 = extended_euclidean_algorithm(240, 46);
  if (($r3['x'] != (-9)) || ($r3['y'] != 47)) {
  $panic('test3 failed');
}
  $r4 = extended_euclidean_algorithm(1, -4);
  if (($r4['x'] != 1) || ($r4['y'] != 0)) {
  $panic('test4 failed');
}
  $r5 = extended_euclidean_algorithm(-2, -4);
  if (($r5['x'] != (-1)) || ($r5['y'] != 0)) {
  $panic('test5 failed');
}
  $r6 = extended_euclidean_algorithm(0, -4);
  if (($r6['x'] != 0) || ($r6['y'] != (-1))) {
  $panic('test6 failed');
}
  $r7 = extended_euclidean_algorithm(2, 0);
  if (($r7['x'] != 1) || ($r7['y'] != 0)) {
  $panic('test7 failed');
}
};
  function main() {
  test_extended_euclidean_algorithm();
  $res = extended_euclidean_algorithm(240, 46);
  echo rtrim('(' . _str($res['x']) . ', ' . _str($res['y']) . ')'), PHP_EOL;
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
