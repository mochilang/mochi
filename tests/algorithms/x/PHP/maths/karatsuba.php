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
  function int_pow($base, $exp) {
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function karatsuba($a, $b) {
  if (strlen(_str($a)) == 1 || strlen(_str($b)) == 1) {
  return $a * $b;
}
  $m1 = strlen(_str($a));
  $lb = strlen(_str($b));
  if ($lb > $m1) {
  $m1 = $lb;
}
  $m2 = _intdiv($m1, 2);
  $power = int_pow(10, $m2);
  $a1 = _intdiv($a, $power);
  $a2 = $a % $power;
  $b1 = _intdiv($b, $power);
  $b2 = $b % $power;
  $x = karatsuba($a2, $b2);
  $y = karatsuba($a1 + $a2, $b1 + $b2);
  $z = karatsuba($a1, $b1);
  $result = $z * int_pow(10, 2 * $m2) + ($y - $z - $x) * $power + $x;
  return $result;
};
  function main() {
  echo rtrim(_str(karatsuba(15463, 23489))), PHP_EOL;
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
