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
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_approx($x) {
  $term = 1.0;
  $sum = 1.0;
  $i = 1;
  while ($i < 20) {
  $term = $term * $x / (floatval($i));
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function softmax($vec) {
  $exps = [];
  $i = 0;
  while ($i < count($vec)) {
  $exps = _append($exps, exp_approx($vec[$i]));
  $i = $i + 1;
};
  $total = 0.0;
  $i = 0;
  while ($i < count($exps)) {
  $total = $total + $exps[$i];
  $i = $i + 1;
};
  $result = [];
  $i = 0;
  while ($i < count($exps)) {
  $result = _append($result, $exps[$i] / $total);
  $i = $i + 1;
};
  return $result;
};
  function abs_val($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function approx_equal($a, $b) {
  return abs_val($a - $b) < 0.0001;
};
  function test_softmax() {
  $s1 = softmax([1.0, 2.0, 3.0, 4.0]);
  $sum1 = 0.0;
  $i = 0;
  while ($i < count($s1)) {
  $sum1 = $sum1 + $s1[$i];
  $i = $i + 1;
};
  if (!approx_equal($sum1, 1.0)) {
  $panic('sum test failed');
}
  $s2 = softmax([5.0, 5.0]);
  if (!(approx_equal($s2[0], 0.5) && approx_equal($s2[1], 0.5))) {
  $panic('equal elements test failed');
}
  $s3 = softmax([0.0]);
  if (!approx_equal($s3[0], 1.0)) {
  $panic('zero vector test failed');
}
};
  function main() {
  test_softmax();
  echo rtrim(_str(softmax([1.0, 2.0, 3.0, 4.0]))), PHP_EOL;
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
