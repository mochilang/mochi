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
  global $samples, $labels, $model;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $sum = $sum + $a[$i] * $b[$i];
  $i = _iadd($i, 1);
};
  return $sum;
};
  function maxf($a, $b) {
  global $samples, $labels, $model;
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function minf($a, $b) {
  global $samples, $labels, $model;
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function absf($x) {
  global $samples, $labels, $model;
  if ($x >= 0.0) {
  return $x;
}
  return 0.0 - $x;
};
  function predict_raw($samples, $labels, $alphas, $b, $x) {
  global $model;
  $res = 0.0;
  $i = 0;
  while ($i < count($samples)) {
  $res = $res + $alphas[$i] * $labels[$i] * dot($samples[$i], $x);
  $i = _iadd($i, 1);
};
  return $res + $b;
};
  function smo_train($samples, $labels, $c, $tol, $max_passes) {
  global $model;
  $m = count($samples);
  $alphas = [];
  $i = 0;
  while ($i < $m) {
  $alphas = _append($alphas, 0.0);
  $i = _iadd($i, 1);
};
  $b = 0.0;
  $passes = 0;
  while ($passes < $max_passes) {
  $num_changed = 0;
  $i1 = 0;
  while ($i1 < $m) {
  $Ei = predict_raw($samples, $labels, $alphas, $b, $samples[$i1]) - $labels[$i1];
  if (($labels[$i1] * $Ei < 0.0 - $tol && $alphas[$i1] < $c) || ($labels[$i1] * $Ei > $tol && $alphas[$i1] > 0.0)) {
  $i2 = _imod((_iadd($i1, 1)), $m);
  $Ej = predict_raw($samples, $labels, $alphas, $b, $samples[$i2]) - $labels[$i2];
  $alpha1_old = $alphas[$i1];
  $alpha2_old = $alphas[$i2];
  $L = 0.0;
  $H = 0.0;
  if ($labels[$i1] != $labels[$i2]) {
  $L = maxf(0.0, $alpha2_old - $alpha1_old);
  $H = minf($c, $c + $alpha2_old - $alpha1_old);
} else {
  $L = maxf(0.0, $alpha2_old + $alpha1_old - $c);
  $H = minf($c, $alpha2_old + $alpha1_old);
};
  if ($L == $H) {
  $i1 = _iadd($i1, 1);
  continue;
};
  $eta = 2.0 * dot($samples[$i1], $samples[$i2]) - dot($samples[$i1], $samples[$i1]) - dot($samples[$i2], $samples[$i2]);
  if ($eta >= 0.0) {
  $i1 = _iadd($i1, 1);
  continue;
};
  $alphas[$i2] = $alpha2_old - $labels[$i2] * ($Ei - $Ej) / $eta;
  if ($alphas[$i2] > $H) {
  $alphas[$i2] = $H;
};
  if ($alphas[$i2] < $L) {
  $alphas[$i2] = $L;
};
  if (absf($alphas[$i2] - $alpha2_old) < 0.00001) {
  $i1 = _iadd($i1, 1);
  continue;
};
  $alphas[$i1] = $alpha1_old + $labels[$i1] * $labels[$i2] * ($alpha2_old - $alphas[$i2]);
  $b1 = $b - $Ei - $labels[$i1] * ($alphas[$i1] - $alpha1_old) * dot($samples[$i1], $samples[$i1]) - $labels[$i2] * ($alphas[$i2] - $alpha2_old) * dot($samples[$i1], $samples[$i2]);
  $b2 = $b - $Ej - $labels[$i1] * ($alphas[$i1] - $alpha1_old) * dot($samples[$i1], $samples[$i2]) - $labels[$i2] * ($alphas[$i2] - $alpha2_old) * dot($samples[$i2], $samples[$i2]);
  if ($alphas[$i1] > 0.0 && $alphas[$i1] < $c) {
  $b = $b1;
} else {
  if ($alphas[$i2] > 0.0 && $alphas[$i2] < $c) {
  $b = $b2;
} else {
  $b = ($b1 + $b2) / 2.0;
};
};
  $num_changed = _iadd($num_changed, 1);
}
  $i1 = _iadd($i1, 1);
};
  if ($num_changed == 0) {
  $passes = _iadd($passes, 1);
} else {
  $passes = 0;
}
};
  return [$alphas, [$b]];
};
  function predict($samples, $labels, $model, $x) {
  $alphas = $model[0];
  $b = $model[1][0];
  $val = predict_raw($samples, $labels, $alphas, $b, $x);
  if ($val >= 0.0) {
  return 1.0;
}
  return -1.0;
};
  $samples = [[2.0, 2.0], [1.5, 1.5], [0.0, 0.0], [0.5, 0.0]];
  $labels = [1.0, 1.0, -1.0, -1.0];
  $model = smo_train($samples, $labels, 1.0, 0.001, 10);
  echo rtrim(json_encode(predict($samples, $labels, $model, [1.5, 1.0]), 1344)), PHP_EOL;
  echo rtrim(json_encode(predict($samples, $labels, $model, [0.2, 0.1]), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
