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
  function mochi_abs($x) {
  global $input_matrix, $vector, $result;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function sqrtApprox($x) {
  global $input_matrix, $vector, $result;
  if ($x == 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function dot($a, $b) {
  global $input_matrix, $vector, $result;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $sum = $sum + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $sum;
};
  function mat_vec_mult($mat, $vec) {
  global $input_matrix, $vector, $result;
  $res = [];
  $i = 0;
  while ($i < count($mat)) {
  $res = _append($res, dot($mat[$i], $vec));
  $i = $i + 1;
};
  return $res;
};
  function norm($vec) {
  global $input_matrix, $vector, $result;
  $sum = 0.0;
  $i = 0;
  while ($i < count($vec)) {
  $sum = $sum + $vec[$i] * $vec[$i];
  $i = $i + 1;
};
  $root = sqrtApprox($sum);
  return $root;
};
  function normalize($vec) {
  global $input_matrix, $vector, $result;
  $n = norm($vec);
  $res = [];
  $i = 0;
  while ($i < count($vec)) {
  $res = _append($res, $vec[$i] / $n);
  $i = $i + 1;
};
  return $res;
};
  function power_iteration($matrix, $vector, $error_tol, $max_iterations) {
  global $input_matrix, $result;
  $v = normalize($vector);
  $lambda_prev = 0.0;
  $lambda = 0.0;
  $err = 1000000000000.0;
  $iterations = 0;
  while ($err > $error_tol && $iterations < $max_iterations) {
  $w = mat_vec_mult($matrix, $v);
  $v = normalize($w);
  $mv = mat_vec_mult($matrix, $v);
  $lambda = dot($v, $mv);
  $denom = ($lambda != 0.0 ? mochi_abs($lambda) : 1.0);
  $err = mochi_abs($lambda - $lambda_prev) / $denom;
  $lambda_prev = $lambda;
  $iterations = $iterations + 1;
};
  return ['eigenvalue' => $lambda, 'eigenvector' => $v];
};
  $input_matrix = [[41.0, 4.0, 20.0], [4.0, 26.0, 30.0], [20.0, 30.0, 50.0]];
  $vector = [41.0, 4.0, 20.0];
  $result = power_iteration($input_matrix, $vector, 0.000000000001, 100);
  echo rtrim(_str($result['eigenvalue'])), PHP_EOL;
  echo rtrim(_str($result['eigenvector'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
