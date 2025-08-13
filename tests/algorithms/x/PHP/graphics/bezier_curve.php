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
  function n_choose_k($n, $k) {
  global $control;
  if ($k < 0 || $k > $n) {
  return 0.0;
}
  if ($k == 0 || $k == $n) {
  return 1.0;
}
  $result = 1.0;
  $i = 1;
  while ($i <= $k) {
  $result = $result * (1.0 * ($n - $k + $i)) / (1.0 * $i);
  $i = $i + 1;
};
  return $result;
};
  function pow_float($base, $exp) {
  global $control;
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function basis_function($points, $t) {
  global $control;
  $degree = count($points) - 1;
  $res = [];
  $i = 0;
  while ($i <= $degree) {
  $coef = n_choose_k($degree, $i);
  $term = pow_float(1.0 - $t, $degree - $i) * pow_float($t, $i);
  $res = _append($res, $coef * $term);
  $i = $i + 1;
};
  return $res;
};
  function bezier_point($points, $t) {
  global $control;
  $basis = basis_function($points, $t);
  $x = 0.0;
  $y = 0.0;
  $i = 0;
  while ($i < count($points)) {
  $x = $x + $basis[$i] * $points[$i][0];
  $y = $y + $basis[$i] * $points[$i][1];
  $i = $i + 1;
};
  return [$x, $y];
};
  $control = [[1.0, 1.0], [1.0, 2.0]];
  echo rtrim(_str(basis_function($control, 0.0))), PHP_EOL;
  echo rtrim(_str(basis_function($control, 1.0))), PHP_EOL;
  echo rtrim(_str(bezier_point($control, 0.0))), PHP_EOL;
  echo rtrim(_str(bezier_point($control, 1.0))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
