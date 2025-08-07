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
  function bubble_sort($xs) {
  $arr = $xs;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < $n - $i - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function factors($num) {
  $values = [1];
  $i = 2;
  while ($i * $i <= $num) {
  if ($num % $i == 0) {
  $values = _append($values, $i);
  $d = _intdiv($num, $i);
  if ($d != $i) {
  $values = _append($values, $d);
};
}
  $i = $i + 1;
};
  return bubble_sort($values);
};
  function sum_list($xs) {
  $total = 0;
  $i = 0;
  while ($i < count($xs)) {
  $total = $total + $xs[$i];
  $i = $i + 1;
};
  return $total;
};
  function abundant($n) {
  return sum_list(factors($n)) > $n;
};
  function semi_perfect($number) {
  if ($number <= 0) {
  return true;
}
  $values = factors($number);
  $possible = [];
  $j = 0;
  while ($j <= $number) {
  $possible = _append($possible, $j == 0);
  $j = $j + 1;
};
  $idx = 0;
  while ($idx < count($values)) {
  $v = $values[$idx];
  $s = $number;
  while ($s >= $v) {
  if ($possible[$s - $v]) {
  $possible[$s] = true;
}
  $s = $s - 1;
};
  $idx = $idx + 1;
};
  return $possible[$number];
};
  function weird($number) {
  return abundant($number) && semi_perfect($number) == false;
};
  function run_tests() {
  if (factors(12) != [1, 2, 3, 4, 6]) {
  $panic('factors 12 failed');
}
  if (factors(1) != [1]) {
  $panic('factors 1 failed');
}
  if (factors(100) != [1, 2, 4, 5, 10, 20, 25, 50]) {
  $panic('factors 100 failed');
}
  if (abundant(0) != true) {
  $panic('abundant 0 failed');
}
  if (abundant(1) != false) {
  $panic('abundant 1 failed');
}
  if (abundant(12) != true) {
  $panic('abundant 12 failed');
}
  if (abundant(13) != false) {
  $panic('abundant 13 failed');
}
  if (abundant(20) != true) {
  $panic('abundant 20 failed');
}
  if (semi_perfect(0) != true) {
  $panic('semi_perfect 0 failed');
}
  if (semi_perfect(1) != true) {
  $panic('semi_perfect 1 failed');
}
  if (semi_perfect(12) != true) {
  $panic('semi_perfect 12 failed');
}
  if (semi_perfect(13) != false) {
  $panic('semi_perfect 13 failed');
}
  if (weird(0) != false) {
  $panic('weird 0 failed');
}
  if (weird(70) != true) {
  $panic('weird 70 failed');
}
  if (weird(77) != false) {
  $panic('weird 77 failed');
}
};
  function main() {
  run_tests();
  $nums = [69, 70, 71];
  $i = 0;
  while ($i < count($nums)) {
  $n = $nums[$i];
  if (weird($n)) {
  echo rtrim(_str($n) . ' is weird.'), PHP_EOL;
} else {
  echo rtrim(_str($n) . ' is not weird.'), PHP_EOL;
}
  $i = $i + 1;
};
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
