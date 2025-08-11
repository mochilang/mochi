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
  function design_matrix($xs, $degree) {
  global $ys, $x, $X, $Xt, $XtX, $Xty, $coeffs;
  $i = 0;
  $matrix = [];
  while ($i < count($xs)) {
  $row = [];
  $j = 0;
  $pow = 1.0;
  while ($j <= $degree) {
  $row = _append($row, $pow);
  $pow = $pow * $xs[$i];
  $j = _iadd($j, 1);
};
  $matrix = _append($matrix, $row);
  $i = _iadd($i, 1);
};
  return $matrix;
};
  function transpose($matrix) {
  global $xs, $ys, $x, $X, $Xt, $XtX, $Xty, $coeffs;
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $j = 0;
  $result = [];
  while ($j < $cols) {
  $row = [];
  $i = 0;
  while ($i < $rows) {
  $row = _append($row, $matrix[$i][$j]);
  $i = _iadd($i, 1);
};
  $result = _append($result, $row);
  $j = _iadd($j, 1);
};
  return $result;
};
  function matmul($A, $B) {
  global $xs, $ys, $x, $X, $Xt, $XtX, $Xty, $coeffs;
  $n = count($A);
  $m = count($A[0]);
  $p = count($B[0]);
  $i = 0;
  $result = [];
  while ($i < $n) {
  $row = [];
  $k = 0;
  while ($k < $p) {
  $sum = 0.0;
  $j = 0;
  while ($j < $m) {
  $sum = $sum + $A[$i][$j] * $B[$j][$k];
  $j = _iadd($j, 1);
};
  $row = _append($row, $sum);
  $k = _iadd($k, 1);
};
  $result = _append($result, $row);
  $i = _iadd($i, 1);
};
  return $result;
};
  function matvec_mul($A, $v) {
  global $xs, $ys, $x, $X, $Xt, $XtX, $Xty, $coeffs;
  $n = count($A);
  $m = count($A[0]);
  $i = 0;
  $result = [];
  while ($i < $n) {
  $sum = 0.0;
  $j = 0;
  while ($j < $m) {
  $sum = $sum + $A[$i][$j] * $v[$j];
  $j = _iadd($j, 1);
};
  $result = _append($result, $sum);
  $i = _iadd($i, 1);
};
  return $result;
};
  function gaussian_elimination($A, $b) {
  global $xs, $ys, $X, $Xt, $XtX, $Xty, $coeffs;
  $n = count($A);
  $M = [];
  $i = 0;
  while ($i < $n) {
  $M = _append($M, _append($A[$i], $b[$i]));
  $i = _iadd($i, 1);
};
  $k = 0;
  while ($k < $n) {
  $j = _iadd($k, 1);
  while ($j < $n) {
  $factor = $M[$j][$k] / $M[$k][$k];
  $rowj = $M[$j];
  $rowk = $M[$k];
  $l = $k;
  while ($l <= $n) {
  $rowj[$l] = $rowj[$l] - $factor * $rowk[$l];
  $l = _iadd($l, 1);
};
  $M[$j] = $rowj;
  $j = _iadd($j, 1);
};
  $k = _iadd($k, 1);
};
  $x = [];
  $t = 0;
  while ($t < $n) {
  $x = _append($x, 0.0);
  $t = _iadd($t, 1);
};
  $i2 = _isub($n, 1);
  while ($i2 >= 0) {
  $sum = $M[$i2][$n];
  $j2 = _iadd($i2, 1);
  while ($j2 < $n) {
  $sum = $sum - $M[$i2][$j2] * $x[$j2];
  $j2 = _iadd($j2, 1);
};
  $x[$i2] = $sum / $M[$i2][$i2];
  $i2 = _isub($i2, 1);
};
  return $x;
};
  function predict($xs, $coeffs) {
  global $ys, $X, $Xt, $XtX, $Xty;
  $i = 0;
  $result = [];
  while ($i < count($xs)) {
  $x = $xs[$i];
  $j = 0;
  $pow = 1.0;
  $sum = 0.0;
  while ($j < count($coeffs)) {
  $sum = $sum + $coeffs[$j] * $pow;
  $pow = $pow * $x;
  $j = _iadd($j, 1);
};
  $result = _append($result, $sum);
  $i = _iadd($i, 1);
};
  return $result;
};
  $xs = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0];
  $ys = [];
  $i = 0;
  while ($i < count($xs)) {
  $x = $xs[$i];
  $ys = _append($ys, $x * $x * $x - 2.0 * $x * $x + 3.0 * $x - 5.0);
  $i = _iadd($i, 1);
}
  $X = design_matrix($xs, 3);
  $Xt = transpose($X);
  $XtX = matmul($Xt, $X);
  $Xty = matvec_mul($Xt, $ys);
  $coeffs = gaussian_elimination($XtX, $Xty);
  echo rtrim(_str($coeffs)), PHP_EOL;
  echo rtrim(_str(predict([-1.0], $coeffs))), PHP_EOL;
  echo rtrim(_str(predict([-2.0], $coeffs))), PHP_EOL;
  echo rtrim(_str(predict([6.0], $coeffs))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
