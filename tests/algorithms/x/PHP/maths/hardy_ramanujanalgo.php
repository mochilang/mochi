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
  function exact_prime_factor_count($n) {
  $count = 0;
  $num = $n;
  if ($num % 2 == 0) {
  $count = $count + 1;
  while ($num % 2 == 0) {
  $num = _intdiv($num, 2);
};
}
  $i = 3;
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  $count = $count + 1;
  while ($num % $i == 0) {
  $num = _intdiv($num, $i);
};
}
  $i = $i + 2;
};
  if ($num > 2) {
  $count = $count + 1;
}
  return $count;
};
  function ln($x) {
  $ln2 = 0.6931471805599453;
  $y = $x;
  $k = 0.0;
  while ($y > 2.0) {
  $y = $y / 2.0;
  $k = $k + $ln2;
};
  while ($y < 1.0) {
  $y = $y * 2.0;
  $k = $k - $ln2;
};
  $t = ($y - 1.0) / ($y + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return $k + 2.0 * $sum;
};
  function mochi_floor($x) {
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function round4($x) {
  $m = 10000.0;
  return mochi_floor($x * $m + 0.5) / $m;
};
  function main() {
  $n = 51242183;
  $count = exact_prime_factor_count($n);
  echo rtrim('The number of distinct prime factors is/are ' . _str($count)), PHP_EOL;
  $loglog = ln(ln(floatval($n)));
  echo rtrim('The value of log(log(n)) is ' . _str(round4($loglog))), PHP_EOL;
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
