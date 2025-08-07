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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function reverse($xs) {
  $res = [];
  $i = count($xs) - 1;
  while ($i >= 0) {
  $res = _append($res, $xs[$i]);
  $i = $i - 1;
};
  return $res;
};
  function factors_of_a_number($num) {
  $facs = [];
  if ($num < 1) {
  return $facs;
}
  $small = [];
  $large = [];
  $i = 1;
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  $small = _append($small, $i);
  $d = _intdiv($num, $i);
  if ($d != $i) {
  $large = _append($large, $d);
};
}
  $i = $i + 1;
};
  $facs = array_merge($small, reverse($large));
  return $facs;
};
  function run_tests() {
  if (factors_of_a_number(1) != [1]) {
  $panic('case1 failed');
}
  if (factors_of_a_number(5) != [1, 5]) {
  $panic('case2 failed');
}
  if (factors_of_a_number(24) != [1, 2, 3, 4, 6, 8, 12, 24]) {
  $panic('case3 failed');
}
  if (factors_of_a_number(-24) != []) {
  $panic('case4 failed');
}
};
  function main() {
  run_tests();
  echo rtrim(_str(factors_of_a_number(24))), PHP_EOL;
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
