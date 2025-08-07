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
  function add($matrices) {
  $rows = count($matrices[0]);
  $cols = count($matrices[0][0]);
  $r = 0;
  $result = [];
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  $sum = 0.0;
  $m = 0;
  while ($m < count($matrices)) {
  $sum = $sum + $matrices[$m][$r][$c];
  $m = $m + 1;
};
  $row = _append($row, $sum);
  $c = $c + 1;
};
  $result = _append($result, $row);
  $r = $r + 1;
};
  return $result;
};
  function subtract($a, $b) {
  $rows = count($a);
  $cols = count($a[0]);
  $r = 0;
  $result = [];
  while ($r < $rows) {
  $row = [];
  $c = 0;
  while ($c < $cols) {
  $row = _append($row, $a[$r][$c] - $b[$r][$c]);
  $c = $c + 1;
};
  $result = _append($result, $row);
  $r = $r + 1;
};
  return $result;
};
  function scalar_multiply($matrix, $n) {
  $result = [];
  $i = 0;
  while ($i < count($matrix)) {
  $row = [];
  $j = 0;
  while ($j < count($matrix[$i])) {
  $row = _append($row, $matrix[$i][$j] * $n);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function multiply($a, $b) {
  $rowsA = count($a);
  $colsA = count($a[0]);
  $rowsB = count($b);
  $colsB = count($b[0]);
  $result = [];
  $i = 0;
  while ($i < $rowsA) {
  $row = [];
  $j = 0;
  while ($j < $colsB) {
  $sum = 0.0;
  $k = 0;
  while ($k < $colsA) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function identity($n) {
  $result = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $result = _append($result, $row);
  $i = $i + 1;
};
  return $result;
};
  function transpose($matrix) {
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $result = [];
  $c = 0;
  while ($c < $cols) {
  $row = [];
  $r = 0;
  while ($r < $rows) {
  $row = _append($row, $matrix[$r][$c]);
  $r = $r + 1;
};
  $result = _append($result, $row);
  $c = $c + 1;
};
  return $result;
};
  function minor($matrix, $row, $column) {
  $result = [];
  $i = 0;
  while ($i < count($matrix)) {
  if ($i != $row) {
  $new_row = [];
  $j = 0;
  while ($j < count($matrix[$i])) {
  if ($j != $column) {
  $new_row = _append($new_row, $matrix[$i][$j]);
}
  $j = $j + 1;
};
  $result = _append($result, $new_row);
}
  $i = $i + 1;
};
  return $result;
};
  function determinant($matrix) {
  if (count($matrix) == 1) {
  return $matrix[0][0];
}
  $det = 0.0;
  $c = 0;
  while ($c < count($matrix[0])) {
  $sub = minor($matrix, 0, $c);
  $sign = ($c % 2 == 0 ? 1.0 : -1.0);
  $det = $det + $matrix[0][$c] * determinant($sub) * $sign;
  $c = $c + 1;
};
  return $det;
};
  function inverse($matrix) {
  $det = determinant($matrix);
  if ($det == 0.0) {
  return [];
}
  $size = count($matrix);
  $matrix_minor = [];
  $i = 0;
  while ($i < $size) {
  $row = [];
  $j = 0;
  while ($j < $size) {
  $m = minor($matrix, $i, $j);
  $row = _append($row, determinant($m));
  $j = $j + 1;
};
  $matrix_minor = _append($matrix_minor, $row);
  $i = $i + 1;
};
  $cofactors = [];
  $i = 0;
  while ($i < $size) {
  $row = [];
  $j = 0;
  while ($j < $size) {
  $sign = (($i + $j) % 2 == 0 ? 1.0 : -1.0);
  $row = _append($row, $matrix_minor[$i][$j] * $sign);
  $j = $j + 1;
};
  $cofactors = _append($cofactors, $row);
  $i = $i + 1;
};
  $adjugate = transpose($cofactors);
  return scalar_multiply($adjugate, 1.0 / $det);
};
  function main() {
  $matrix_a = [[12.0, 10.0], [3.0, 9.0]];
  $matrix_b = [[3.0, 4.0], [7.0, 4.0]];
  $matrix_c = [[11.0, 12.0, 13.0, 14.0], [21.0, 22.0, 23.0, 24.0], [31.0, 32.0, 33.0, 34.0], [41.0, 42.0, 43.0, 44.0]];
  $matrix_d = [[3.0, 0.0, 2.0], [2.0, 0.0, -2.0], [0.0, 1.0, 1.0]];
  echo rtrim('Add Operation, add(matrix_a, matrix_b) = ' . _str(add([$matrix_a, $matrix_b])) . ' 
'), PHP_EOL;
  echo rtrim('Multiply Operation, multiply(matrix_a, matrix_b) = ' . _str(multiply($matrix_a, $matrix_b)) . ' 
'), PHP_EOL;
  echo rtrim('Identity: ' . _str(identity(5)) . '
'), PHP_EOL;
  echo rtrim('Minor of ' . _str($matrix_c) . ' = ' . _str(minor($matrix_c, 1, 2)) . ' 
'), PHP_EOL;
  echo rtrim('Determinant of ' . _str($matrix_b) . ' = ' . _str(determinant($matrix_b)) . ' 
'), PHP_EOL;
  echo rtrim('Inverse of ' . _str($matrix_d) . ' = ' . _str(inverse($matrix_d)) . '
'), PHP_EOL;
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
