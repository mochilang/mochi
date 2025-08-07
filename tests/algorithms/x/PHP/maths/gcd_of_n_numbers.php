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
  function gcd($a, $b) {
  $x = $a;
  $y = $b;
  while ($y != 0) {
  $r = $x % $y;
  $x = $y;
  $y = $r;
};
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function get_greatest_common_divisor($nums) {
  if (count($nums) == 0) {
  $panic('at least one number is required');
}
  $g = $nums[0];
  if ($g <= 0) {
  $panic('numbers must be integer and greater than zero');
}
  $i = 1;
  while ($i < count($nums)) {
  $n = $nums[$i];
  if ($n <= 0) {
  $panic('numbers must be integer and greater than zero');
}
  $g = gcd($g, $n);
  $i = $i + 1;
};
  return $g;
};
  echo rtrim(_str(get_greatest_common_divisor([18, 45]))), PHP_EOL;
  echo rtrim(_str(get_greatest_common_divisor([23, 37]))), PHP_EOL;
  echo rtrim(_str(get_greatest_common_divisor([2520, 8350]))), PHP_EOL;
  echo rtrim(_str(get_greatest_common_divisor([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
