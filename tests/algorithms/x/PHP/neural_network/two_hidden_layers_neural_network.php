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
  $sum = 1.0;
  $term = 1.0;
  $i = 1;
  while ($i < 10) {
  $term = $term * $x / floatval($i);
  $sum = $sum + $term;
  $i = $i + 1;
};
  return $sum;
};
  function sigmoid($x) {
  return 1.0 / (1.0 + exp_approx(-$x));
};
  function sigmoid_derivative($x) {
  return $x * (1.0 - $x);
};
  function new_network() {
  return ['w1' => [[0.1, 0.2, 0.3, 0.4], [0.5, 0.6, 0.7, 0.8], [0.9, 1.0, 1.1, 1.2]], 'w2' => [[0.1, 0.2, 0.3], [0.4, 0.5, 0.6], [0.7, 0.8, 0.9], [1.0, 1.1, 1.2]], 'w3' => [[0.1], [0.2], [0.3]]];
};
  function feedforward($net, $input) {
  $hidden1 = [];
  $j = 0;
  while ($j < 4) {
  $sum1 = 0.0;
  $i = 0;
  while ($i < 3) {
  $sum1 = $sum1 + $input[$i] * $net['w1'][$i][$j];
  $i = $i + 1;
};
  $hidden1 = _append($hidden1, sigmoid($sum1));
  $j = $j + 1;
};
  $hidden2 = [];
  $k = 0;
  while ($k < 3) {
  $sum2 = 0.0;
  $j2 = 0;
  while ($j2 < 4) {
  $sum2 = $sum2 + $hidden1[$j2] * $net['w2'][$j2][$k];
  $j2 = $j2 + 1;
};
  $hidden2 = _append($hidden2, sigmoid($sum2));
  $k = $k + 1;
};
  $sum3 = 0.0;
  $k2 = 0;
  while ($k2 < 3) {
  $sum3 = $sum3 + $hidden2[$k2] * $net['w3'][$k2][0];
  $k2 = $k2 + 1;
};
  $out = sigmoid($sum3);
  return $out;
};
  function train(&$net, $inputs, $outputs, $iterations) {
  $iter = 0;
  while ($iter < $iterations) {
  $s = 0;
  while ($s < count($inputs)) {
  $inp = $inputs[$s];
  $target = $outputs[$s];
  $hidden1 = [];
  $j = 0;
  while ($j < 4) {
  $sum1 = 0.0;
  $i = 0;
  while ($i < 3) {
  $sum1 = $sum1 + $inp[$i] * $net['w1'][$i][$j];
  $i = $i + 1;
};
  $hidden1 = _append($hidden1, sigmoid($sum1));
  $j = $j + 1;
};
  $hidden2 = [];
  $k = 0;
  while ($k < 3) {
  $sum2 = 0.0;
  $j2 = 0;
  while ($j2 < 4) {
  $sum2 = $sum2 + $hidden1[$j2] * $net['w2'][$j2][$k];
  $j2 = $j2 + 1;
};
  $hidden2 = _append($hidden2, sigmoid($sum2));
  $k = $k + 1;
};
  $sum3 = 0.0;
  $k3 = 0;
  while ($k3 < 3) {
  $sum3 = $sum3 + $hidden2[$k3] * $net['w3'][$k3][0];
  $k3 = $k3 + 1;
};
  $output = sigmoid($sum3);
  $error = $target - $output;
  $delta_output = $error * sigmoid_derivative($output);
  $new_w3 = [];
  $k4 = 0;
  while ($k4 < 3) {
  $w3row = $net['w3'][$k4];
  $w3row[0] = $w3row[0] + $hidden2[$k4] * $delta_output;
  $new_w3 = _append($new_w3, $w3row);
  $k4 = $k4 + 1;
};
  $net['w3'] = $new_w3;
  $delta_hidden2 = [];
  $k5 = 0;
  while ($k5 < 3) {
  $row = $net['w3'][$k5];
  $dh2 = $row[0] * $delta_output * sigmoid_derivative($hidden2[$k5]);
  $delta_hidden2 = _append($delta_hidden2, $dh2);
  $k5 = $k5 + 1;
};
  $new_w2 = [];
  $j = 0;
  while ($j < 4) {
  $w2row = $net['w2'][$j];
  $k6 = 0;
  while ($k6 < 3) {
  $w2row[$k6] = $w2row[$k6] + $hidden1[$j] * $delta_hidden2[$k6];
  $k6 = $k6 + 1;
};
  $new_w2 = _append($new_w2, $w2row);
  $j = $j + 1;
};
  $net['w2'] = $new_w2;
  $delta_hidden1 = [];
  $j = 0;
  while ($j < 4) {
  $sumdh = 0.0;
  $k7 = 0;
  while ($k7 < 3) {
  $row2 = $net['w2'][$j];
  $sumdh = $sumdh + $row2[$k7] * $delta_hidden2[$k7];
  $k7 = $k7 + 1;
};
  $delta_hidden1 = _append($delta_hidden1, $sumdh * sigmoid_derivative($hidden1[$j]));
  $j = $j + 1;
};
  $new_w1 = [];
  $i2 = 0;
  while ($i2 < 3) {
  $w1row = $net['w1'][$i2];
  $j = 0;
  while ($j < 4) {
  $w1row[$j] = $w1row[$j] + $inp[$i2] * $delta_hidden1[$j];
  $j = $j + 1;
};
  $new_w1 = _append($new_w1, $w1row);
  $i2 = $i2 + 1;
};
  $net['w1'] = $new_w1;
  $s = $s + 1;
};
  $iter = $iter + 1;
};
};
  function predict($net, $input) {
  $out = feedforward($net, $input);
  if ($out > 0.6) {
  return 1;
}
  return 0;
};
  function example() {
  $inputs = [[0.0, 0.0, 0.0], [0.0, 0.0, 1.0], [0.0, 1.0, 0.0], [0.0, 1.0, 1.0], [1.0, 0.0, 0.0], [1.0, 0.0, 1.0], [1.0, 1.0, 0.0], [1.0, 1.0, 1.0]];
  $outputs = [0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0];
  $net = new_network();
  train($net, $inputs, $outputs, 10);
  $result = predict($net, [1.0, 1.0, 1.0]);
  echo rtrim(_str($result)), PHP_EOL;
  return $result;
};
  function main() {
  example();
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
