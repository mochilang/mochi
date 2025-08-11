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
  function dot($a, $b) {
  global $xs, $ys, $base, $model;
  $s = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $s = $s + $a[$i] * $b[$i];
  $i = _iadd($i, 1);
};
  return $s;
};
  function new_svc($lr, $lambda, $epochs) {
  global $xs, $ys, $base, $model;
  return ['weights' => [], 'bias' => 0.0, 'lr' => $lr, 'lambda' => $lambda, 'epochs' => $epochs];
};
  function fit($model, $xs, $ys) {
  global $base;
  $n_features = count($xs[0]);
  $w = [];
  $i = 0;
  while ($i < $n_features) {
  $w = _append($w, 0.0);
  $i = _iadd($i, 1);
};
  $b = 0.0;
  $epoch = 0;
  while ($epoch < $model['epochs']) {
  $j = 0;
  while ($j < count($xs)) {
  $x = $xs[$j];
  $y = floatval($ys[$j]);
  $prod = dot($w, $x) + $b;
  if ($y * $prod < 1.0) {
  $k = 0;
  while ($k < count($w)) {
  $w[$k] = $w[$k] + $model['lr'] * ($y * $x[$k] - 2.0 * $model['lambda'] * $w[$k]);
  $k = _iadd($k, 1);
};
  $b = $b + $model['lr'] * $y;
} else {
  $k = 0;
  while ($k < count($w)) {
  $w[$k] = $w[$k] - $model['lr'] * (2.0 * $model['lambda'] * $w[$k]);
  $k = _iadd($k, 1);
};
}
  $j = _iadd($j, 1);
};
  $epoch = _iadd($epoch, 1);
};
  return ['weights' => $w, 'bias' => $b, 'lr' => $model['lr'], 'lambda' => $model['lambda'], 'epochs' => $model['epochs']];
};
  function predict($model, $x) {
  global $xs, $ys, $base;
  $s = dot($model['weights'], $x) + $model['bias'];
  if ($s >= 0.0) {
  return 1;
} else {
  return -1;
}
};
  $xs = [[0.0, 1.0], [0.0, 2.0], [1.0, 1.0], [1.0, 2.0]];
  $ys = [1, 1, -1, -1];
  $base = new_svc(0.01, 0.01, 1000);
  $model = fit($base, $xs, $ys);
  echo rtrim(json_encode(predict($model, [0.0, 1.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(predict($model, [1.0, 1.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(predict($model, [2.0, 2.0]), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
