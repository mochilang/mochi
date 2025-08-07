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
  function extended_euclid($a, $b) {
  global $e1, $e2;
  if ($b == 0) {
  return ['x' => 1, 'y' => 0];
}
  $res = extended_euclid($b, $a % $b);
  $k = _intdiv($a, $b);
  return ['x' => $res['y'], 'y' => $res['x'] - $k * $res['y']];
};
  function chinese_remainder_theorem($n1, $r1, $n2, $r2) {
  global $e1, $e2;
  $res = extended_euclid($n1, $n2);
  $x = $res['x'];
  $y = $res['y'];
  $m = $n1 * $n2;
  $n = $r2 * $x * $n1 + $r1 * $y * $n2;
  return (($n % $m) + $m) % $m;
};
  function invert_modulo($a, $n) {
  global $e1, $e2;
  $res = extended_euclid($a, $n);
  $b = $res['x'];
  if ($b < 0) {
  $b = ($b % $n + $n) % $n;
}
  return $b;
};
  function chinese_remainder_theorem2($n1, $r1, $n2, $r2) {
  global $e1, $e2;
  $x = invert_modulo($n1, $n2);
  $y = invert_modulo($n2, $n1);
  $m = $n1 * $n2;
  $n = $r2 * $x * $n1 + $r1 * $y * $n2;
  return (($n % $m) + $m) % $m;
};
  $e1 = extended_euclid(10, 6);
  echo rtrim(_str($e1['x']) . ',' . _str($e1['y'])), PHP_EOL;
  $e2 = extended_euclid(7, 5);
  echo rtrim(_str($e2['x']) . ',' . _str($e2['y'])), PHP_EOL;
  echo rtrim(_str(chinese_remainder_theorem(5, 1, 7, 3))), PHP_EOL;
  echo rtrim(_str(chinese_remainder_theorem(6, 1, 4, 3))), PHP_EOL;
  echo rtrim(_str(invert_modulo(2, 5))), PHP_EOL;
  echo rtrim(_str(invert_modulo(8, 7))), PHP_EOL;
  echo rtrim(_str(chinese_remainder_theorem2(5, 1, 7, 3))), PHP_EOL;
  echo rtrim(_str(chinese_remainder_theorem2(6, 1, 4, 3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
