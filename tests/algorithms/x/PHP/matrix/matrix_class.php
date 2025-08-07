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
  function make_matrix($values) {
  $r = count($values);
  if ($r == 0) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $c = count($values[0]);
  $i = 0;
  while ($i < $r) {
  if (count($values[$i]) != $c) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $i = $i + 1;
};
  return ['data' => $values, 'rows' => $r, 'cols' => $c];
};
  function matrix_columns($m) {
  $cols = [];
  $j = 0;
  while ($j < $m['cols']) {
  $col = [];
  $i = 0;
  while ($i < $m['rows']) {
  $col = _append($col, $m['data'][$i][$j]);
  $i = $i + 1;
};
  $cols = _append($cols, $col);
  $j = $j + 1;
};
  return $cols;
};
  function matrix_identity($m) {
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $v = ($i == $j ? 1.0 : 0.0);
  $row = _append($row, $v);
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_minor($m, $r, $c) {
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  if ($i != $r) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  if ($j != $c) {
  $row = _append($row, $m['data'][$i][$j]);
}
  $j = $j + 1;
};
  $vals = _append($vals, $row);
}
  $i = $i + 1;
};
  $sub = ['data' => $vals, 'rows' => $m['rows'] - 1, 'cols' => $m['cols'] - 1];
  return matrix_determinant($sub);
};
  function matrix_cofactor($m, $r, $c) {
  $minor = matrix_minor($m, $r, $c);
  if (($r + $c) % 2 == 0) {
  return $minor;
}
  return -1.0 * $minor;
};
  function matrix_minors($m) {
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $row = _append($row, matrix_minor($m, $i, $j));
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_cofactors($m) {
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $row = _append($row, matrix_cofactor($m, $i, $j));
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_determinant($m) {
  if ($m['rows'] != $m['cols']) {
  return 0.0;
}
  if ($m['rows'] == 0) {
  return 0.0;
}
  if ($m['rows'] == 1) {
  return $m['data'][0][0];
}
  if ($m['rows'] == 2) {
  return $m['data'][0][0] * $m['data'][1][1] - $m['data'][0][1] * $m['data'][1][0];
}
  $sum = 0.0;
  $j = 0;
  while ($j < $m['cols']) {
  $sum = $sum + $m['data'][0][$j] * matrix_cofactor($m, 0, $j);
  $j = $j + 1;
};
  return $sum;
};
  function matrix_is_invertible($m) {
  return matrix_determinant($m) != 0.0;
};
  function matrix_adjugate($m) {
  $cof = matrix_cofactors($m);
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $row = _append($row, $cof['data'][$j][$i]);
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_inverse($m) {
  $det = matrix_determinant($m);
  if ($det == 0.0) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $adj = matrix_adjugate($m);
  return matrix_mul_scalar($adj, 1.0 / $det);
};
  function matrix_add_row($m, $row) {
  $newData = $m['data'];
  $newData = _append($newData, $row);
  return ['data' => $newData, 'rows' => $m['rows'] + 1, 'cols' => $m['cols']];
};
  function matrix_add_column($m, $col) {
  $newData = [];
  $i = 0;
  while ($i < $m['rows']) {
  $newData = _append($newData, _append($m['data'][$i], $col[$i]));
  $i = $i + 1;
};
  return ['data' => $newData, 'rows' => $m['rows'], 'cols' => $m['cols'] + 1];
};
  function matrix_mul_scalar($m, $s) {
  $vals = [];
  $i = 0;
  while ($i < $m['rows']) {
  $row = [];
  $j = 0;
  while ($j < $m['cols']) {
  $row = _append($row, $m['data'][$i][$j] * $s);
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $m['rows'], 'cols' => $m['cols']];
};
  function matrix_neg($m) {
  return matrix_mul_scalar($m, -1.0);
};
  function matrix_add($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $vals = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $a['cols']) {
  $row = _append($row, $a['data'][$i][$j] + $b['data'][$i][$j]);
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $a['rows'], 'cols' => $a['cols']];
};
  function matrix_sub($a, $b) {
  if ($a['rows'] != $b['rows'] || $a['cols'] != $b['cols']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $vals = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $a['cols']) {
  $row = _append($row, $a['data'][$i][$j] - $b['data'][$i][$j]);
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $a['rows'], 'cols' => $a['cols']];
};
  function matrix_dot($row, $col) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($row)) {
  $sum = $sum + $row[$i] * $col[$i];
  $i = $i + 1;
};
  return $sum;
};
  function matrix_mul($a, $b) {
  if ($a['cols'] != $b['rows']) {
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $bcols = matrix_columns($b);
  $vals = [];
  $i = 0;
  while ($i < $a['rows']) {
  $row = [];
  $j = 0;
  while ($j < $b['cols']) {
  $row = _append($row, matrix_dot($a['data'][$i], $bcols[$j]));
  $j = $j + 1;
};
  $vals = _append($vals, $row);
  $i = $i + 1;
};
  return ['data' => $vals, 'rows' => $a['rows'], 'cols' => $b['cols']];
};
  function matrix_pow($m, $p) {
  if ($p == 0) {
  return matrix_identity($m);
}
  if ($p < 0) {
  if (matrix_is_invertible($m)) {
  return matrix_pow(matrix_inverse($m), -$p);
};
  return ['data' => [], 'rows' => 0, 'cols' => 0];
}
  $result = $m;
  $i = 1;
  while ($i < $p) {
  $result = matrix_mul($result, $m);
  $i = $i + 1;
};
  return $result;
};
  function matrix_to_string($m) {
  if ($m['rows'] == 0) {
  return '[]';
}
  $s = '[';
  $i = 0;
  while ($i < $m['rows']) {
  $s = $s . '[';
  $j = 0;
  while ($j < $m['cols']) {
  $s = $s . _str($m['data'][$i][$j]);
  if ($j < $m['cols'] - 1) {
  $s = $s . ' ';
}
  $j = $j + 1;
};
  $s = $s . ']';
  if ($i < $m['rows'] - 1) {
  $s = $s . '
 ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function main() {
  $m = make_matrix([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]);
  echo rtrim(matrix_to_string($m)), PHP_EOL;
  echo rtrim(_str(matrix_columns($m))), PHP_EOL;
  echo rtrim(_str($m['rows']) . ',' . _str($m['cols'])), PHP_EOL;
  echo rtrim(_str(matrix_is_invertible($m))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_identity($m))), PHP_EOL;
  echo rtrim(_str(matrix_determinant($m))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_minors($m))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_cofactors($m))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_adjugate($m))), PHP_EOL;
  $m2 = matrix_mul_scalar($m, 3.0);
  echo rtrim(matrix_to_string($m2)), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_add($m, $m2))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_sub($m, $m2))), PHP_EOL;
  echo rtrim(matrix_to_string(matrix_pow($m, 3))), PHP_EOL;
  $m3 = matrix_add_row($m, [10.0, 11.0, 12.0]);
  echo rtrim(matrix_to_string($m3)), PHP_EOL;
  $m4 = matrix_add_column($m2, [8.0, 16.0, 32.0]);
  echo rtrim(matrix_to_string(matrix_mul($m3, $m4))), PHP_EOL;
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
