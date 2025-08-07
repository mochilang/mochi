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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_approx($x) {
  $sum = 1.0;
  $term = 1.0;
  $i = 1;
  while ($i <= 20) {
  $term = $term * $x / (floatval($i));
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function sigmoid($vector) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $v = $vector[$i];
  $s = 1.0 / (1.0 + exp_approx(-$v));
  $result = _append($result, $s);
  $i = $i + 1;
};
  return $result;
};
  function swish($vector, $beta) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $v = $vector[$i];
  $s = 1.0 / (1.0 + exp_approx(-$beta * $v));
  $result = _append($result, $v * $s);
  $i = $i + 1;
};
  return $result;
};
  function sigmoid_linear_unit($vector) {
  return swish($vector, 1.0);
};
  function approx_equal($a, $b, $eps) {
  $diff = ($a > $b ? $a - $b : $b - $a);
  return $diff < $eps;
};
  function approx_equal_list($a, $b, $eps) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if (!approx_equal($a[$i], $b[$i], $eps)) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function test_swish() {
  $v = [-1.0, 1.0, 2.0];
  $eps = 0.001;
  if (!approx_equal_list(sigmoid($v), [0.26894142, 0.73105858, 0.88079708], $eps)) {
  _panic('sigmoid incorrect');
}
  if (!approx_equal_list(sigmoid_linear_unit($v), [-0.26894142, 0.73105858, 1.76159416], $eps)) {
  _panic('sigmoid_linear_unit incorrect');
}
  if (!approx_equal_list(swish($v, 2.0), [-0.11920292, 0.88079708, 1.96402758], $eps)) {
  _panic('swish incorrect');
}
  if (!approx_equal_list(swish([-2.0], 1.0), [-0.23840584], $eps)) {
  _panic('swish with parameter 1 incorrect');
}
};
  function main() {
  test_swish();
  echo rtrim(_str(sigmoid([-1.0, 1.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(sigmoid_linear_unit([-1.0, 1.0, 2.0]))), PHP_EOL;
  echo rtrim(_str(swish([-1.0, 1.0, 2.0], 2.0))), PHP_EOL;
  echo rtrim(_str(swish([-2.0], 1.0))), PHP_EOL;
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
