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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function exp_taylor($x) {
  global $X, $Y, $test_data, $w1, $b1, $w2, $b2, $preds;
  $term = 1.0;
  $sum = 1.0;
  $i = 1.0;
  while ($i < 20.0) {
  $term = $term * $x / $i;
  $sum = $sum + $term;
  $i = $i + 1.0;
};
  return $sum;
};
  function sigmoid($x) {
  global $X, $Y, $test_data, $w1, $b1, $w2, $b2, $preds;
  return 1.0 / (1.0 + exp_taylor(-$x));
};
  $X = [[0.0, 0.0], [1.0, 1.0], [1.0, 0.0], [0.0, 1.0]];
  $Y = [0.0, 1.0, 0.0, 0.0];
  $test_data = [[0.0, 0.0], [0.0, 1.0], [1.0, 1.0]];
  $w1 = [[0.5, -0.5], [0.5, 0.5]];
  $b1 = [0.0, 0.0];
  $w2 = [0.5, -0.5];
  $b2 = 0.0;
  function train($epochs, $lr) {
  global $X, $Y, $test_data, $w1, $b1, $w2, $b2, $preds;
  $e = 0;
  while ($e < $epochs) {
  $i = 0;
  while ($i < count($X)) {
  $x0 = $X[$i][0];
  $x1 = $X[$i][1];
  $target = $Y[$i];
  $z1 = $w1[0][0] * $x0 + $w1[1][0] * $x1 + $b1[0];
  $z2 = $w1[0][1] * $x0 + $w1[1][1] * $x1 + $b1[1];
  $h1 = sigmoid($z1);
  $h2 = sigmoid($z2);
  $z3 = $w2[0] * $h1 + $w2[1] * $h2 + $b2;
  $out = sigmoid($z3);
  $error = $out - $target;
  $d1 = $h1 * (1.0 - $h1) * $w2[0] * $error;
  $d2 = $h2 * (1.0 - $h2) * $w2[1] * $error;
  $w2[0] = $w2[0] - $lr * $error * $h1;
  $w2[1] = $w2[1] - $lr * $error * $h2;
  $b2 = $b2 - $lr * $error;
  $w1[0][0] = $w1[0][0] - $lr * $d1 * $x0;
  $w1[1][0] = $w1[1][0] - $lr * $d1 * $x1;
  $b1[0] = $b1[0] - $lr * $d1;
  $w1[0][1] = $w1[0][1] - $lr * $d2 * $x0;
  $w1[1][1] = $w1[1][1] - $lr * $d2 * $x1;
  $b1[1] = $b1[1] - $lr * $d2;
  $i = _iadd($i, 1);
};
  $e = _iadd($e, 1);
};
};
  function predict($samples) {
  global $X, $Y, $test_data, $w1, $b1, $w2, $b2;
  $preds = [];
  $i = 0;
  while ($i < count($samples)) {
  $x0 = $samples[$i][0];
  $x1 = $samples[$i][1];
  $z1 = $w1[0][0] * $x0 + $w1[1][0] * $x1 + $b1[0];
  $z2 = $w1[0][1] * $x0 + $w1[1][1] * $x1 + $b1[1];
  $h1 = sigmoid($z1);
  $h2 = sigmoid($z2);
  $z3 = $w2[0] * $h1 + $w2[1] * $h2 + $b2;
  $out = sigmoid($z3);
  $label = 0;
  if ($out >= 0.5) {
  $label = 1;
}
  $preds = _append($preds, $label);
  $i = _iadd($i, 1);
};
  return $preds;
};
  function wrapper($y) {
  global $X, $Y, $test_data, $w1, $b1, $w2, $b2, $preds;
  return $y;
};
  train(4000, 0.5);
  $preds = wrapper(predict($test_data));
  echo rtrim(_str($preds)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
