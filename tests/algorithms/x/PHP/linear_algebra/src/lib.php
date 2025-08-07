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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
  $PI = 3.141592653589793;
  $seed = 123456789;
  function mochi_rand() {
  global $PI, $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function mochi_random_int($a, $b) {
  global $PI, $seed;
  $r = fmod(mochi_rand(), ($b - $a + 1));
  return $a + $r;
};
  function sqrtApprox($x) {
  global $PI, $seed;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function arcsin_taylor($x) {
  global $PI, $seed;
  $term = $x;
  $sum = $x;
  $n = 1;
  while ($n < 10) {
  $num = (2.0 * (floatval($n)) - 1.0) * (2.0 * (floatval($n)) - 1.0) * $x * $x * $term;
  $den = (2.0 * (floatval($n))) * (2.0 * (floatval($n)) + 1.0);
  $term = $num / $den;
  $sum = $sum + $term;
  $n = $n + 1;
};
  return $sum;
};
  function acos_taylor($x) {
  global $PI, $seed;
  return $PI / 2.0 - arcsin_taylor($x);
};
  function vector_len($v) {
  global $PI, $seed;
  return _len($v['components']);
};
  function vector_to_string($v) {
  global $PI, $seed;
  $s = '(';
  $i = 0;
  while ($i < _len($v['components'])) {
  $s = $s . _str($v['components'][$i]);
  if ($i < _len($v['components']) - 1) {
  $s = $s . ',';
}
  $i = $i + 1;
};
  $s = $s . ')';
  return $s;
};
  function vector_add($a, $b) {
  global $PI, $seed;
  $size = vector_len($a);
  if ($size != vector_len($b)) {
  return ['components' => []];
}
  $res = [];
  $i = 0;
  while ($i < $size) {
  $res = _append($res, $a['components'][$i] + $b['components'][$i]);
  $i = $i + 1;
};
  return ['components' => $res];
};
  function vector_sub($a, $b) {
  global $PI, $seed;
  $size = vector_len($a);
  if ($size != vector_len($b)) {
  return ['components' => []];
}
  $res = [];
  $i = 0;
  while ($i < $size) {
  $res = _append($res, $a['components'][$i] - $b['components'][$i]);
  $i = $i + 1;
};
  return ['components' => $res];
};
  function vector_eq($a, $b) {
  global $PI, $seed;
  if (vector_len($a) != vector_len($b)) {
  return false;
}
  $i = 0;
  while ($i < vector_len($a)) {
  if ($a['components'][$i] != $b['components'][$i]) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function vector_mul_scalar($v, $s) {
  global $PI, $seed;
  $res = [];
  $i = 0;
  while ($i < vector_len($v)) {
  $res = _append($res, $v['components'][$i] * $s);
  $i = $i + 1;
};
  return ['components' => $res];
};
  function vector_dot($a, $b) {
  global $PI, $seed;
  $size = vector_len($a);
  if ($size != vector_len($b)) {
  return 0.0;
}
  $sum = 0.0;
  $i = 0;
  while ($i < $size) {
  $sum = $sum + $a['components'][$i] * $b['components'][$i];
  $i = $i + 1;
};
  return $sum;
};
  function vector_copy($v) {
  global $PI, $seed;
  $res = [];
  $i = 0;
  while ($i < vector_len($v)) {
  $res = _append($res, $v['components'][$i]);
  $i = $i + 1;
};
  return ['components' => $res];
};
  function vector_component($v, $idx) {
  global $PI, $seed;
  return $v['components'][$idx];
};
  function vector_change_component($v, $pos, $value) {
  global $PI, $seed;
  $comps = $v['components'];
  $comps[$pos] = $value;
  return ['components' => $comps];
};
  function vector_euclidean_length($v) {
  global $PI, $seed;
  $sum = 0.0;
  $i = 0;
  while ($i < _len($v['components'])) {
  $sum = $sum + $v['components'][$i] * $v['components'][$i];
  $i = $i + 1;
};
  $result = sqrtApprox($sum);
  return $result;
};
  function vector_angle($a, $b, $deg) {
  global $PI, $seed;
  $num = vector_dot($a, $b);
  $den = vector_euclidean_length($a) * vector_euclidean_length($b);
  $ang = acos_taylor($num / $den);
  if ($deg) {
  $ang = $ang * 180.0 / $PI;
}
  return $ang;
};
  function zero_vector($d) {
  global $PI, $seed;
  $res = [];
  $i = 0;
  while ($i < $d) {
  $res = _append($res, 0.0);
  $i = $i + 1;
};
  return ['components' => $res];
};
  function unit_basis_vector($d, $pos) {
  global $PI, $seed;
  $res = [];
  $i = 0;
  while ($i < $d) {
  if ($i == $pos) {
  $res = _append($res, 1.0);
} else {
  $res = _append($res, 0.0);
}
  $i = $i + 1;
};
  return ['components' => $res];
};
  function axpy($s, $x, $y) {
  global $PI, $seed;
  return vector_add(vector_mul_scalar($x, $s), $y);
};
  function random_vector($n, $a, $b) {
  global $PI, $seed;
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, floatval(mochi_random_int($a, $b)));
  $i = $i + 1;
};
  return ['components' => $res];
};
  function matrix_to_string($m) {
  global $PI, $seed;
  $ans = '';
  $i = 0;
  while ($i < $m['height']) {
  $ans = $ans . '|';
  $j = 0;
  while ($j < $m['width']) {
  $ans = $ans . _str($m['data'][$i][$j]);
  if ($j < $m['width'] - 1) {
  $ans = $ans . ',';
}
  $j = $j + 1;
};
  $ans = $ans . '|
';
  $i = $i + 1;
};
  return $ans;
};
  function matrix_add($a, $b) {
  global $PI, $seed;
  if ($a['width'] != $b['width'] || $a['height'] != $b['height']) {
  return ['data' => [], 'width' => 0, 'height' => 0];
}
  $mat = [];
  $i = 0;
  while ($i < $a['height']) {
  $row = [];
  $j = 0;
  while ($j < $a['width']) {
  $row = _append($row, $a['data'][$i][$j] + $b['data'][$i][$j]);
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return ['data' => $mat, 'width' => $a['width'], 'height' => $a['height']];
};
  function matrix_sub($a, $b) {
  global $PI, $seed;
  if ($a['width'] != $b['width'] || $a['height'] != $b['height']) {
  return ['data' => [], 'width' => 0, 'height' => 0];
}
  $mat = [];
  $i = 0;
  while ($i < $a['height']) {
  $row = [];
  $j = 0;
  while ($j < $a['width']) {
  $row = _append($row, $a['data'][$i][$j] - $b['data'][$i][$j]);
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return ['data' => $mat, 'width' => $a['width'], 'height' => $a['height']];
};
  function matrix_mul_vector($m, $v) {
  global $PI, $seed;
  if (_len($v['components']) != $m['width']) {
  return ['components' => []];
}
  $res = zero_vector($m['height']);
  $i = 0;
  while ($i < $m['height']) {
  $sum = 0.0;
  $j = 0;
  while ($j < $m['width']) {
  $sum = $sum + $m['data'][$i][$j] * $v['components'][$j];
  $j = $j + 1;
};
  $res = vector_change_component($res, $i, $sum);
  $i = $i + 1;
};
  return $res;
};
  function matrix_mul_scalar($m, $s) {
  global $PI, $seed;
  $mat = [];
  $i = 0;
  while ($i < $m['height']) {
  $row = [];
  $j = 0;
  while ($j < $m['width']) {
  $row = _append($row, $m['data'][$i][$j] * $s);
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return ['data' => $mat, 'width' => $m['width'], 'height' => $m['height']];
};
  function matrix_component($m, $x, $y) {
  global $PI, $seed;
  return $m['data'][$x][$y];
};
  function matrix_change_component($m, $x, $y, $value) {
  global $PI, $seed;
  $data = $m['data'];
  $data[$x][$y] = $value;
  return ['data' => $data, 'width' => $m['width'], 'height' => $m['height']];
};
  function matrix_minor($m, $x, $y) {
  global $PI, $seed;
  if ($m['height'] != $m['width']) {
  return 0.0;
}
  $minor = [];
  $i = 0;
  while ($i < $m['height']) {
  if ($i != $x) {
  $row = [];
  $j = 0;
  while ($j < $m['width']) {
  if ($j != $y) {
  $row = _append($row, $m['data'][$i][$j]);
}
  $j = $j + 1;
};
  $minor = _append($minor, $row);
}
  $i = $i + 1;
};
  $sub = ['data' => $minor, 'width' => $m['width'] - 1, 'height' => $m['height'] - 1];
  return matrix_determinant($sub);
};
  function matrix_cofactor($m, $x, $y) {
  global $PI, $seed;
  $sign = (($x + $y) % 2 == 0 ? 1.0 : -1.0);
  return $sign * matrix_minor($m, $x, $y);
};
  function matrix_determinant($m) {
  global $PI, $seed;
  if ($m['height'] != $m['width']) {
  return 0.0;
}
  if ($m['height'] == 0) {
  return 0.0;
}
  if ($m['height'] == 1) {
  return $m['data'][0][0];
}
  if ($m['height'] == 2) {
  return $m['data'][0][0] * $m['data'][1][1] - $m['data'][0][1] * $m['data'][1][0];
}
  $sum = 0.0;
  $y = 0;
  while ($y < $m['width']) {
  $sum = $sum + $m['data'][0][$y] * matrix_cofactor($m, 0, $y);
  $y = $y + 1;
};
  return $sum;
};
  function square_zero_matrix($n) {
  global $PI, $seed;
  $mat = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0.0);
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return ['data' => $mat, 'width' => $n, 'height' => $n];
};
  function random_matrix($w, $h, $a, $b) {
  global $PI, $seed;
  $mat = [];
  $i = 0;
  while ($i < $h) {
  $row = [];
  $j = 0;
  while ($j < $w) {
  $row = _append($row, floatval(mochi_random_int($a, $b)));
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return ['data' => $mat, 'width' => $w, 'height' => $h];
};
  function main() {
  global $PI, $seed;
  $v1 = ['components' => [1.0, 2.0, 3.0]];
  $v2 = ['components' => [4.0, 5.0, 6.0]];
  echo rtrim(vector_to_string(vector_add($v1, $v2))), PHP_EOL;
  echo rtrim(_str(vector_dot($v1, $v2))), PHP_EOL;
  echo rtrim(_str(vector_euclidean_length($v1))), PHP_EOL;
  $m = ['data' => [[1.0, 2.0], [3.0, 4.0]], 'width' => 2, 'height' => 2];
  echo rtrim(_str(matrix_determinant($m))), PHP_EOL;
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
