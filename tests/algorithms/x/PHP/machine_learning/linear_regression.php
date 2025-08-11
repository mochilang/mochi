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
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
  function dot($x, $y) {
  global $data_x, $data_y, $theta, $predicted_y, $original_y, $mae;
  $sum = 0.0;
  $i = 0;
  while ($i < count($x)) {
  $sum = $sum + $x[$i] * $y[$i];
  $i = _iadd($i, 1);
};
  return $sum;
};
  function run_steep_gradient_descent($data_x, $data_y, $len_data, $alpha, $theta) {
  global $predicted_y, $original_y, $mae;
  $gradients = [];
  $j = 0;
  while ($j < count($theta)) {
  $gradients = _append($gradients, 0.0);
  $j = _iadd($j, 1);
};
  $i = 0;
  while ($i < $len_data) {
  $prediction = dot($theta, $data_x[$i]);
  $error = $prediction - $data_y[$i];
  $k = 0;
  while ($k < count($theta)) {
  $gradients[$k] = $gradients[$k] + $error * $data_x[$i][$k];
  $k = _iadd($k, 1);
};
  $i = _iadd($i, 1);
};
  $t = [];
  $g = 0;
  while ($g < count($theta)) {
  $t = _append($t, _isub($theta[$g], _imul((_idiv($alpha, $len_data)), $gradients[$g])));
  $g = _iadd($g, 1);
};
  return $t;
};
  function sum_of_square_error($data_x, $data_y, $len_data, $theta) {
  global $predicted_y, $original_y, $mae;
  $total = 0.0;
  $i = 0;
  while ($i < $len_data) {
  $prediction = dot($theta, $data_x[$i]);
  $diff = $prediction - $data_y[$i];
  $total = $total + $diff * $diff;
  $i = _iadd($i, 1);
};
  return _idiv($total, (_imul(2.0, $len_data)));
};
  function run_linear_regression($data_x, $data_y) {
  global $predicted_y, $original_y, $mae;
  $iterations = 10;
  $alpha = 0.01;
  $no_features = count($data_x[0]);
  $len_data = count($data_x);
  $theta = [];
  $i = 0;
  while ($i < $no_features) {
  $theta = _append($theta, 0.0);
  $i = _iadd($i, 1);
};
  $iter = 0;
  while ($iter < $iterations) {
  $theta = run_steep_gradient_descent($data_x, $data_y, $len_data, $alpha, $theta);
  $error = sum_of_square_error($data_x, $data_y, $len_data, $theta);
  echo rtrim('At Iteration ' . _str(_iadd($iter, 1)) . ' - Error is ' . _str($error)), PHP_EOL;
  $iter = _iadd($iter, 1);
};
  return $theta;
};
  function absf($x) {
  global $data_x, $data_y, $theta, $i, $predicted_y, $original_y, $mae;
  if ($x < 0.0) {
  return -$x;
} else {
  return $x;
}
};
  function mean_absolute_error($predicted_y, $original_y) {
  global $data_x, $data_y, $theta, $mae;
  $total = 0.0;
  $i = 0;
  while ($i < count($predicted_y)) {
  $diff = absf($predicted_y[$i] - $original_y[$i]);
  $total = $total + $diff;
  $i = _iadd($i, 1);
};
  return _idiv($total, count($predicted_y));
};
  $data_x = [[1.0, 1.0], [1.0, 2.0], [1.0, 3.0]];
  $data_y = [1.0, 2.0, 3.0];
  $theta = run_linear_regression($data_x, $data_y);
  echo rtrim('Resultant Feature vector :'), PHP_EOL;
  $i = 0;
  while ($i < count($theta)) {
  echo rtrim(_str($theta[$i])), PHP_EOL;
  $i = _iadd($i, 1);
}
  $predicted_y = [3.0, -0.5, 2.0, 7.0];
  $original_y = [2.5, 0.0, 2.0, 8.0];
  $mae = mean_absolute_error($predicted_y, $original_y);
  echo rtrim('Mean Absolute Error : ' . _str($mae)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
