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
  function remove_at($xs, $idx) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function kth_permutation($k, $n) {
  if ($n <= 0) {
  $panic('n must be positive');
}
  $factorials = [1];
  $i = 2;
  while ($i < $n) {
  $factorials = _append($factorials, $factorials[count($factorials) - 1] * $i);
  $i = $i + 1;
};
  $total = $factorials[count($factorials) - 1] * $n;
  if (($k < 0) || ($k >= $total)) {
  $panic('k out of bounds');
}
  $elements = [];
  $e = 0;
  while ($e < $n) {
  $elements = _append($elements, $e);
  $e = $e + 1;
};
  $permutation = [];
  $idx = count($factorials) - 1;
  while ($idx >= 0) {
  $factorial = $factorials[$idx];
  $number = _intdiv($k, $factorial);
  $k = $k % $factorial;
  $permutation = _append($permutation, $elements[$number]);
  $elements = remove_at($elements, $number);
  $idx = $idx - 1;
};
  $permutation = _append($permutation, $elements[0]);
  return $permutation;
};
  function list_equal($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function list_to_string($xs) {
  if (count($xs) == 0) {
  return '[]';
}
  $s = '[' . _str($xs[0]);
  $i = 1;
  while ($i < count($xs)) {
  $s = $s . ', ' . _str($xs[$i]);
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function test_kth_permutation() {
  $expected1 = [0, 1, 2, 3, 4];
  $res1 = kth_permutation(0, 5);
  if (!list_equal($res1, $expected1)) {
  $panic('test case 1 failed');
}
  $expected2 = [1, 3, 0, 2];
  $res2 = kth_permutation(10, 4);
  if (!list_equal($res2, $expected2)) {
  $panic('test case 2 failed');
}
};
  function main() {
  test_kth_permutation();
  $res = kth_permutation(10, 4);
  echo rtrim(list_to_string($res)), PHP_EOL;
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
