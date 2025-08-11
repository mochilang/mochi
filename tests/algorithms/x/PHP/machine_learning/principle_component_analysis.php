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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_sqrt($x) {
  global $data, $result, $idx;
  $guess = ($x > 1.0 ? $x / 2.0 : 1.0);
  $i = 0;
  while ($i < 20) {
  $guess = 0.5 * ($guess + $x / $guess);
  $i = _iadd($i, 1);
};
  return $guess;
};
  function mean($xs) {
  global $data, $result, $idx;
  $sum = 0.0;
  $i = 0;
  while ($i < count($xs)) {
  $sum = $sum + $xs[$i];
  $i = _iadd($i, 1);
};
  return _idiv($sum, count($xs));
};
  function standardize($data) {
  global $result, $idx;
  $n_samples = count($data);
  $n_features = count($data[0]);
  $means = [];
  $stds = [];
  $j = 0;
  while ($j < $n_features) {
  $column = [];
  $i = 0;
  while ($i < $n_samples) {
  $column = _append($column, $data[$i][$j]);
  $i = _iadd($i, 1);
};
  $m = mean($column);
  $means = _append($means, $m);
  $variance = 0.0;
  $k = 0;
  while ($k < $n_samples) {
  $diff = $column[$k] - $m;
  $variance = $variance + $diff * $diff;
  $k = _iadd($k, 1);
};
  $stds = _append($stds, mochi_sqrt(_idiv($variance, (_isub($n_samples, 1)))));
  $j = _iadd($j, 1);
};
  $standardized = [];
  $r = 0;
  while ($r < $n_samples) {
  $row = [];
  $c = 0;
  while ($c < $n_features) {
  $row = _append($row, ($data[$r][$c] - $means[$c]) / $stds[$c]);
  $c = _iadd($c, 1);
};
  $standardized = _append($standardized, $row);
  $r = _iadd($r, 1);
};
  return $standardized;
};
  function covariance_matrix($data) {
  global $result, $idx;
  $n_samples = count($data);
  $n_features = count($data[0]);
  $cov = [];
  $i = 0;
  while ($i < $n_features) {
  $row = [];
  $j = 0;
  while ($j < $n_features) {
  $sum = 0.0;
  $k = 0;
  while ($k < $n_samples) {
  $sum = $sum + $data[$k][$i] * $data[$k][$j];
  $k = _iadd($k, 1);
};
  $row = _append($row, _idiv($sum, (_isub($n_samples, 1))));
  $j = _iadd($j, 1);
};
  $cov = _append($cov, $row);
  $i = _iadd($i, 1);
};
  return $cov;
};
  function normalize($vec) {
  global $data, $result, $idx;
  $sum = 0.0;
  $i = 0;
  while ($i < count($vec)) {
  $sum = $sum + $vec[$i] * $vec[$i];
  $i = _iadd($i, 1);
};
  $n = mochi_sqrt($sum);
  $res = [];
  $j = 0;
  while ($j < count($vec)) {
  $res = _append($res, $vec[$j] / $n);
  $j = _iadd($j, 1);
};
  return $res;
};
  function eigen_decomposition_2x2($matrix) {
  global $data, $result, $idx;
  $a = $matrix[0][0];
  $b = $matrix[0][1];
  $c = $matrix[1][1];
  $diff = $a - $c;
  $discriminant = mochi_sqrt($diff * $diff + 4.0 * $b * $b);
  $lambda1 = ($a + $c + $discriminant) / 2.0;
  $lambda2 = ($a + $c - $discriminant) / 2.0;
  $v1 = null;
  $v2 = null;
  if ($b != 0.0) {
  $v1 = normalize([$lambda1 - $c, $b]);
  $v2 = normalize([$lambda2 - $c, $b]);
} else {
  $v1 = [1.0, 0.0];
  $v2 = [0.0, 1.0];
}
  $eigenvalues = [$lambda1, $lambda2];
  $eigenvectors = [$v1, $v2];
  if ($eigenvalues[0] < $eigenvalues[1]) {
  $tmp_val = $eigenvalues[0];
  $eigenvalues[0] = $eigenvalues[1];
  $eigenvalues[1] = $tmp_val;
  $tmp_vec = $eigenvectors[0];
  $eigenvectors[0] = $eigenvectors[1];
  $eigenvectors[1] = $tmp_vec;
}
  return ['values' => $eigenvalues, 'vectors' => $eigenvectors];
};
  function transpose($matrix) {
  global $data, $result, $idx;
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $trans = [];
  $i = 0;
  while ($i < $cols) {
  $row = [];
  $j = 0;
  while ($j < $rows) {
  $row = _append($row, $matrix[$j][$i]);
  $j = _iadd($j, 1);
};
  $trans = _append($trans, $row);
  $i = _iadd($i, 1);
};
  return $trans;
};
  function matrix_multiply($a, $b) {
  global $data, $idx;
  $rows_a = count($a);
  $cols_a = count($a[0]);
  $rows_b = count($b);
  $cols_b = count($b[0]);
  if ($cols_a != $rows_b) {
  _panic('Incompatible matrices');
}
  $result = [];
  $i = 0;
  while ($i < $rows_a) {
  $row = [];
  $j = 0;
  while ($j < $cols_b) {
  $sum = 0.0;
  $k = 0;
  while ($k < $cols_a) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = _iadd($k, 1);
};
  $row = _append($row, $sum);
  $j = _iadd($j, 1);
};
  $result = _append($result, $row);
  $i = _iadd($i, 1);
};
  return $result;
};
  function apply_pca($data, $n_components) {
  global $result, $idx;
  $standardized = standardize($data);
  $cov = covariance_matrix($standardized);
  $eig = eigen_decomposition_2x2($cov);
  $eigenvalues = $eig['values'];
  $eigenvectors = $eig['vectors'];
  $components = transpose($eigenvectors);
  $transformed = matrix_multiply($standardized, $components);
  $total = $eigenvalues[0] + $eigenvalues[1];
  $ratios = [];
  $i = 0;
  while ($i < $n_components) {
  $ratios = _append($ratios, $eigenvalues[$i] / $total);
  $i = _iadd($i, 1);
};
  return ['transformed' => $transformed, 'variance_ratio' => $ratios];
};
  $data = [[2.5, 2.4], [0.5, 0.7], [2.2, 2.9], [1.9, 2.2], [3.1, 3.0], [2.3, 2.7], [2.0, 1.6], [1.0, 1.1], [1.5, 1.6], [1.1, 0.9]];
  $result = apply_pca($data, 2);
  echo rtrim('Transformed Data (first 5 rows):'), PHP_EOL;
  $idx = 0;
  while ($idx < 5) {
  echo rtrim(json_encode($result['transformed'][$idx], 1344)), PHP_EOL;
  $idx = _iadd($idx, 1);
}
  echo rtrim('Explained Variance Ratio:'), PHP_EOL;
  echo rtrim(json_encode($result['variance_ratio'], 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
