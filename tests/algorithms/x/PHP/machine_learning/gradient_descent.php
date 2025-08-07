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
  function absf($x) {
  global $train_data, $test_data, $parameter_vector;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function hypothesis_value($input, $params) {
  global $train_data, $test_data, $parameter_vector;
  $value = $params[0];
  $i = 0;
  while ($i < count($input)) {
  $value = $value + $input[$i] * $params[$i + 1];
  $i = $i + 1;
};
  return $value;
};
  function calc_error($dp, $params) {
  global $train_data, $test_data, $parameter_vector;
  return hypothesis_value($dp['x'], $params) - $dp['y'];
};
  function summation_of_cost_derivative($index, $params, $data) {
  global $train_data, $test_data, $parameter_vector;
  $sum = 0.0;
  $i = 0;
  while ($i < count($data)) {
  $dp = $data[$i];
  $e = calc_error($dp, $params);
  if ($index == (-1)) {
  $sum = $sum + $e;
} else {
  $sum = $sum + $e * $dp['x'][$index];
}
  $i = $i + 1;
};
  return $sum;
};
  function get_cost_derivative($index, $params, $data) {
  global $train_data, $test_data, $parameter_vector;
  return summation_of_cost_derivative($index, $params, $data) / (floatval(count($data)));
};
  function allclose($a, $b, $atol, $rtol) {
  global $train_data, $test_data, $parameter_vector;
  $i = 0;
  while ($i < count($a)) {
  $diff = absf($a[$i] - $b[$i]);
  $limit = $atol + $rtol * absf($b[$i]);
  if ($diff > $limit) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function run_gradient_descent($train_data, $initial_params) {
  global $test_data, $parameter_vector;
  $learning_rate = 0.009;
  $absolute_error_limit = 0.000002;
  $relative_error_limit = 0.0;
  $j = 0;
  $params = $initial_params;
  while (true) {
  $j = $j + 1;
  $temp = [];
  $i = 0;
  while ($i < count($params)) {
  $deriv = get_cost_derivative($i - 1, $params, $train_data);
  $temp = _append($temp, $params[$i] - $learning_rate * $deriv);
  $i = $i + 1;
};
  if (allclose($params, $temp, $absolute_error_limit, $relative_error_limit)) {
  echo rtrim('Number of iterations:' . _str($j)), PHP_EOL;
  break;
}
  $params = $temp;
};
  return $params;
};
  function test_gradient_descent($test_data, $params) {
  global $train_data, $parameter_vector;
  $i = 0;
  while ($i < count($test_data)) {
  $dp = $test_data[$i];
  echo rtrim('Actual output value:' . _str($dp['y'])), PHP_EOL;
  echo rtrim('Hypothesis output:' . _str(hypothesis_value($dp['x'], $params))), PHP_EOL;
  $i = $i + 1;
};
};
  $train_data = [['x' => [5.0, 2.0, 3.0], 'y' => 15.0], ['x' => [6.0, 5.0, 9.0], 'y' => 25.0], ['x' => [11.0, 12.0, 13.0], 'y' => 41.0], ['x' => [1.0, 1.0, 1.0], 'y' => 8.0], ['x' => [11.0, 12.0, 13.0], 'y' => 41.0]];
  $test_data = [['x' => [515.0, 22.0, 13.0], 'y' => 555.0], ['x' => [61.0, 35.0, 49.0], 'y' => 150.0]];
  $parameter_vector = [2.0, 4.0, 1.0, 5.0];
  $parameter_vector = run_gradient_descent($train_data, $parameter_vector);
  echo rtrim('
Testing gradient descent for a linear hypothesis function.
'), PHP_EOL;
  test_gradient_descent($test_data, $parameter_vector);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
