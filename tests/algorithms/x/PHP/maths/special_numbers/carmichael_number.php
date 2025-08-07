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
  function abs_int($x) {
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function gcd($a, $b) {
  if ($a == 0) {
  return abs_int($b);
}
  return gcd($b % $a, $a);
};
  function power($x, $y, $m) {
  if ($y == 0) {
  return 1 % $m;
}
  $temp = fmod(power($x, _intdiv($y, 2), $m), $m);
  $temp = ($temp * $temp) % $m;
  if ($y % 2 == 1) {
  $temp = ($temp * $x) % $m;
}
  return $temp;
};
  function is_carmichael_number($n) {
  if ($n <= 0) {
  $panic('Number must be positive');
}
  $b = 2;
  while ($b < $n) {
  if (gcd($b, $n) == 1) {
  if (power($b, $n - 1, $n) != 1) {
  return false;
};
}
  $b = $b + 1;
};
  return true;
};
  echo rtrim(_str(power(2, 15, 3))), PHP_EOL;
  echo rtrim(_str(power(5, 1, 30))), PHP_EOL;
  echo rtrim(_str(is_carmichael_number(4))), PHP_EOL;
  echo rtrim(_str(is_carmichael_number(561))), PHP_EOL;
  echo rtrim(_str(is_carmichael_number(562))), PHP_EOL;
  echo rtrim(_str(is_carmichael_number(1105))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
