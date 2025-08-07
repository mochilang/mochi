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
  function abs_int($n) {
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function num_digits($n) {
  $x = abs_int($n);
  $digits = 1;
  while ($x >= 10) {
  $x = _intdiv($x, 10);
  $digits = $digits + 1;
};
  return $digits;
};
  function num_digits_fast($n) {
  $x = abs_int($n);
  $digits = 1;
  $power = 10;
  while ($x >= $power) {
  $power = $power * 10;
  $digits = $digits + 1;
};
  return $digits;
};
  function num_digits_faster($n) {
  $s = _str(abs_int($n));
  return strlen($s);
};
  function test_num_digits() {
  if (num_digits(12345) != 5) {
  $panic('num_digits 12345 failed');
}
  if (num_digits(123) != 3) {
  $panic('num_digits 123 failed');
}
  if (num_digits(0) != 1) {
  $panic('num_digits 0 failed');
}
  if (num_digits(-1) != 1) {
  $panic('num_digits -1 failed');
}
  if (num_digits(-123456) != 6) {
  $panic('num_digits -123456 failed');
}
  if (num_digits_fast(12345) != 5) {
  $panic('num_digits_fast 12345 failed');
}
  if (num_digits_fast(123) != 3) {
  $panic('num_digits_fast 123 failed');
}
  if (num_digits_fast(0) != 1) {
  $panic('num_digits_fast 0 failed');
}
  if (num_digits_fast(-1) != 1) {
  $panic('num_digits_fast -1 failed');
}
  if (num_digits_fast(-123456) != 6) {
  $panic('num_digits_fast -123456 failed');
}
  if (num_digits_faster(12345) != 5) {
  $panic('num_digits_faster 12345 failed');
}
  if (num_digits_faster(123) != 3) {
  $panic('num_digits_faster 123 failed');
}
  if (num_digits_faster(0) != 1) {
  $panic('num_digits_faster 0 failed');
}
  if (num_digits_faster(-1) != 1) {
  $panic('num_digits_faster -1 failed');
}
  if (num_digits_faster(-123456) != 6) {
  $panic('num_digits_faster -123456 failed');
}
};
  function main() {
  test_num_digits();
  echo rtrim(_str(num_digits(12345))), PHP_EOL;
  echo rtrim(_str(num_digits_fast(12345))), PHP_EOL;
  echo rtrim(_str(num_digits_faster(12345))), PHP_EOL;
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
