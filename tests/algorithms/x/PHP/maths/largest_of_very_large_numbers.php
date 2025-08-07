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
  function ln($x) {
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $k = 1;
  while ($k <= 99) {
  $sum = $sum + $term / (floatval($k));
  $term = $term * $t * $t;
  $k = $k + 2;
};
  return 2.0 * $sum;
};
  function log10($x) {
  return ln($x) / ln(10.0);
};
  function absf($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function res($x, $y) {
  if ($x == 0) {
  return 0.0;
}
  if ($y == 0) {
  return 1.0;
}
  if ($x < 0) {
  $panic('math domain error');
}
  return (floatval($y)) * log10(floatval($x));
};
  function test_res() {
  if (absf(res(5, 7) - 4.892790030352132) > 0.0000001) {
  $panic('res(5,7) failed');
}
  if (res(0, 5) != 0.0) {
  $panic('res(0,5) failed');
}
  if (res(3, 0) != 1.0) {
  $panic('res(3,0) failed');
}
};
  function compare($x1, $y1, $x2, $y2) {
  $r1 = res($x1, $y1);
  $r2 = res($x2, $y2);
  if ($r1 > $r2) {
  return 'Largest number is ' . _str($x1) . ' ^ ' . _str($y1);
}
  if ($r2 > $r1) {
  return 'Largest number is ' . _str($x2) . ' ^ ' . _str($y2);
}
  return 'Both are equal';
};
  test_res();
  echo rtrim(compare(5, 7, 4, 8)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
