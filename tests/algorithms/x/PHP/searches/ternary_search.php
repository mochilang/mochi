<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  $precision = 10;
  function lin_search($left, $right, $array, $target) {
  global $precision;
  $i = $left;
  while ($i < $right) {
  if ($array[$i] == $target) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function ite_ternary_search($array, $target) {
  global $precision;
  $left = 0;
  $right = count($array) - 1;
  while ($left <= $right) {
  if ($right - $left < $precision) {
  $idx = lin_search($left, $right + 1, $array, $target);
  return $idx;
}
  $one_third = $left + _intdiv(($right - $left), 3);
  $two_third = $right - _intdiv(($right - $left), 3);
  if ($array[$one_third] == $target) {
  return $one_third;
}
  if ($array[$two_third] == $target) {
  return $two_third;
}
  if ($target < $array[$one_third]) {
  $right = $one_third - 1;
} else {
  if ($array[$two_third] < $target) {
  $left = $two_third + 1;
} else {
  $left = $one_third + 1;
  $right = $two_third - 1;
};
}
};
  return -1;
};
  function rec_ternary_search($left, $right, $array, $target) {
  global $precision;
  if ($left <= $right) {
  if ($right - $left < $precision) {
  $idx = lin_search($left, $right + 1, $array, $target);
  return $idx;
};
  $one_third = $left + _intdiv(($right - $left), 3);
  $two_third = $right - _intdiv(($right - $left), 3);
  if ($array[$one_third] == $target) {
  return $one_third;
};
  if ($array[$two_third] == $target) {
  return $two_third;
};
  if ($target < $array[$one_third]) {
  return rec_ternary_search($left, $one_third - 1, $array, $target);
};
  if ($array[$two_third] < $target) {
  return rec_ternary_search($two_third + 1, $right, $array, $target);
};
  return rec_ternary_search($one_third + 1, $two_third - 1, $array, $target);
}
  return -1;
};
  function main() {
  global $precision;
  $test_list = [0, 1, 2, 8, 13, 17, 19, 32, 42];
  echo rtrim(_str(ite_ternary_search($test_list, 3))), PHP_EOL;
  echo rtrim(_str(ite_ternary_search($test_list, 13))), PHP_EOL;
  echo rtrim(_str(rec_ternary_search(0, count($test_list) - 1, $test_list, 3))), PHP_EOL;
  echo rtrim(_str(rec_ternary_search(0, count($test_list) - 1, $test_list, 13))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
