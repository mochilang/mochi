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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function intSqrt($x) {
  if ($x < 2) {
  return $x;
}
  $left = 1;
  $right = _intdiv($x, 2);
  $ans = 0;
  while ($left <= $right) {
  $mid = $left + _intdiv(($right - $left), 2);
  $sq = $mid * $mid;
  if ($sq == $x) {
  return $mid;
}
  if ($sq < $x) {
  $left = $mid + 1;
  $ans = $mid;
} else {
  $right = $mid - 1;
}
};
  return $ans;
};
  function sumRecip($n) {
  $s = 1;
  $limit = intSqrt($n);
  $f = 2;
  while ($f <= $limit) {
  if ($n % $f == 0) {
  $s = $s + _intdiv($n, $f);
  $f2 = _intdiv($n, $f);
  if ($f2 != $f) {
  $s = $s + $f;
};
}
  $f = $f + 1;
};
  return $s;
};
  function main() {
  $nums = [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776];
  foreach ($nums as $n) {
  $s = sumRecip($n);
  if ($s % $n == 0) {
  $val = _intdiv($s, $n);
  $perfect = '';
  if ($val == 1) {
  $perfect = 'perfect!';
};
  echo rtrim('Sum of recipr. factors of ' . _str($n) . ' = ' . _str($val) . ' exactly ' . $perfect), PHP_EOL;
}
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
