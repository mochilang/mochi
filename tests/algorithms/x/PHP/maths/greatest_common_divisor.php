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
  function abs_int($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function greatest_common_divisor($a, $b) {
  $x = abs_int($a);
  $y = abs_int($b);
  if ($x == 0) {
  return $y;
}
  return greatest_common_divisor($y % $x, $x);
};
  function gcd_by_iterative($x, $y) {
  $a = abs_int($x);
  $b = abs_int($y);
  while ($b != 0) {
  $temp = $b;
  $b = $a % $b;
  $a = $temp;
};
  return $a;
};
  echo rtrim(_str(greatest_common_divisor(24, 40))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(1, 1))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(1, 800))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(11, 37))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(3, 5))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(16, 4))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(-3, 9))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(9, -3))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(3, -9))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(-3, -9))), PHP_EOL;
  echo rtrim(_str(gcd_by_iterative(24, 40))), PHP_EOL;
  echo rtrim(_str(greatest_common_divisor(24, 40) == gcd_by_iterative(24, 40))), PHP_EOL;
  echo rtrim(_str(gcd_by_iterative(-3, -9))), PHP_EOL;
  echo rtrim(_str(gcd_by_iterative(3, -9))), PHP_EOL;
  echo rtrim(_str(gcd_by_iterative(1, -800))), PHP_EOL;
  echo rtrim(_str(gcd_by_iterative(11, 37))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
