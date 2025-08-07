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
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function int_to_string($n) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  if ($n == 0) {
  return '0';
}
  $num = $n;
  $neg = false;
  if ($num < 0) {
  $neg = true;
  $num = -$num;
}
  $res = '';
  while ($num > 0) {
  $digit = $num % 10;
  $ch = substr('0123456789', $digit, $digit + 1 - $digit);
  $res = $ch . $res;
  $num = _intdiv($num, 10);
};
  if ($neg) {
  $res = '-' . $res;
}
  return $res;
};
  function float_to_string($x, $dec) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $neg = false;
  $num = $x;
  if ($num < 0.0) {
  $neg = true;
  $num = -$num;
}
  $int_part = intval($num);
  $res = int_to_string($int_part);
  if ($dec > 0) {
  $res = $res . '.';
  $frac = $num - (floatval($int_part));
  $i = 0;
  while ($i < $dec) {
  $frac = $frac * 10.0;
  $digit = intval($frac);
  $res = $res . substr('0123456789', $digit, $digit + 1 - $digit);
  $frac = $frac - (floatval($digit));
  $i = $i + 1;
};
}
  if ($neg) {
  $res = '-' . $res;
}
  return $res;
};
  function vector_component($v, $i) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  return $v[$i];
};
  function vector_str_int($v) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $s = '(';
  $i = 0;
  while ($i < count($v)) {
  $s = $s . int_to_string($v[$i]);
  if ($i + 1 < count($v)) {
  $s = $s . ',';
}
  $i = $i + 1;
};
  $s = $s . ')';
  return $s;
};
  function vector_str_float($v, $dec) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $s = '(';
  $i = 0;
  while ($i < count($v)) {
  $s = $s . float_to_string($v[$i], $dec);
  if ($i + 1 < count($v)) {
  $s = $s . ',';
}
  $i = $i + 1;
};
  $s = $s . ')';
  return $s;
};
  function vector_add($a, $b) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] + $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function vector_sub($a, $b) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $res = _append($res, $a[$i] - $b[$i]);
  $i = $i + 1;
};
  return $res;
};
  function vector_scalar_mul($v, $s) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($v)) {
  $res = _append($res, (floatval($v[$i])) * $s);
  $i = $i + 1;
};
  return $res;
};
  function vector_dot($a, $b) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $sum = 0;
  $i = 0;
  while ($i < count($a)) {
  $sum = $sum + $a[$i] * $b[$i];
  $i = $i + 1;
};
  return $sum;
};
  function sqrt_newton($x) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  if ($x == 0.0) {
  return 0.0;
}
  $low = 0.0;
  $high = $x;
  if ($x < 1.0) {
  $high = 1.0;
}
  $mid = 0.0;
  $i = 0;
  while ($i < 40) {
  $mid = ($low + $high) / 2.0;
  if ($mid * $mid > $x) {
  $high = $mid;
} else {
  $low = $mid;
}
  $i = $i + 1;
};
  return $mid;
};
  function euclidean_length($v) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $sum = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $val = floatval($v[$i]);
  $sum = $sum + $val * $val;
  $i = $i + 1;
};
  return sqrt_newton($sum);
};
  function zero_vector($n) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $v = [];
  $i = 0;
  while ($i < $n) {
  $v = _append($v, 0);
  $i = $i + 1;
};
  return $v;
};
  function unit_basis_vector($n, $idx) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $v = zero_vector($n);
  $v[$idx] = 1;
  return $v;
};
  function axpy($a, $x, $y) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($x)) {
  $res = _append($res, $a * $x[$i] + $y[$i]);
  $i = $i + 1;
};
  return $res;
};
  function copy_vector($x) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($x)) {
  $res = _append($res, $x[$i]);
  $i = $i + 1;
};
  return $res;
};
  function change_component(&$v, $idx, $val) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $v[$idx] = $val;
};
  function matrix_str($m) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $s = '';
  $i = 0;
  while ($i < count($m)) {
  $s = $s . '|';
  $j = 0;
  while ($j < count($m[0])) {
  $s = $s . int_to_string($m[$i][$j]);
  if ($j + 1 < count($m[0])) {
  $s = $s . ',';
}
  $j = $j + 1;
};
  $s = $s . '|
';
  $i = $i + 1;
};
  return $s;
};
  function submatrix($m, $row, $col) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  if ($i != $row) {
  $r = [];
  $j = 0;
  while ($j < count($m[0])) {
  if ($j != $col) {
  $r = _append($r, $m[$i][$j]);
}
  $j = $j + 1;
};
  $res = _append($res, $r);
}
  $i = $i + 1;
};
  return $res;
};
  function determinant($m) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $n = count($m);
  if ($n == 1) {
  return $m[0][0];
}
  if ($n == 2) {
  return $m[0][0] * $m[1][1] - $m[0][1] * $m[1][0];
}
  $det = 0;
  $c = 0;
  while ($c < $n) {
  $sub = submatrix($m, 0, $c);
  $sign = 1;
  if ($c % 2 == 1) {
  $sign = -1;
}
  $det = $det + $sign * $m[0][$c] * determinant($sub);
  $c = $c + 1;
};
  return $det;
};
  function matrix_minor($m, $row, $col) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  return determinant(submatrix($m, $row, $col));
};
  function matrix_cofactor($m, $row, $col) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $sign = 1;
  if (($row + $col) % 2 == 1) {
  $sign = -1;
}
  return $sign * matrix_minor($m, $row, $col);
};
  function matrix_mul_vector($m, $v) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $sum = 0;
  $j = 0;
  while ($j < count($m[0])) {
  $sum = $sum + $m[$i][$j] * $v[$j];
  $j = $j + 1;
};
  $res = _append($res, $sum);
  $i = $i + 1;
};
  return $res;
};
  function matrix_mul_scalar($m, $s) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($m)) {
  $row = [];
  $j = 0;
  while ($j < count($m[0])) {
  $row = _append($row, $m[$i][$j] * $s);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function matrix_change_component(&$m, $i, $j, $val) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $m[$i][$j] = $val;
};
  function matrix_component($m, $i, $j) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  return $m[$i][$j];
};
  function matrix_add($a, $b) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($a[0])) {
  $row = _append($row, $a[$i][$j] + $b[$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function matrix_sub($a, $b) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $res = [];
  $i = 0;
  while ($i < count($a)) {
  $row = [];
  $j = 0;
  while ($j < count($a[0])) {
  $row = _append($row, $a[$i][$j] - $b[$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function square_zero_matrix($n) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $m = [];
  $i = 0;
  while ($i < $n) {
  $m = _append($m, zero_vector($n));
  $i = $i + 1;
};
  return $m;
};
  function assert_int($name, $actual, $expected) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  if ($actual == $expected) {
  echo rtrim($name . ' ok'), PHP_EOL;
} else {
  echo rtrim($name . ' fail ' . int_to_string($actual) . ' != ' . int_to_string($expected)), PHP_EOL;
}
};
  function assert_str($name, $actual, $expected) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  if ($actual == $expected) {
  echo rtrim($name . ' ok'), PHP_EOL;
} else {
  echo rtrim($name . ' fail'), PHP_EOL;
  echo rtrim($actual), PHP_EOL;
  echo rtrim($expected), PHP_EOL;
}
};
  function assert_float($name, $actual, $expected, $eps) {
  global $vx, $vs, $vsize, $va, $vb, $vsum, $vsub, $vmul, $zvec, $zstr, $zcount, $zi, $vcopy, $vchange, $ma, $mb, $mv, $msc, $mc, $madd, $msub, $mzero;
  $diff = $actual - $expected;
  if ($diff < 0.0) {
  $diff = -$diff;
}
  if ($diff <= $eps) {
  echo rtrim($name . ' ok'), PHP_EOL;
} else {
  echo rtrim($name . ' fail'), PHP_EOL;
}
};
  $vx = [1, 2, 3];
  assert_int('component0', vector_component($vx, 0), 1);
  assert_int('component2', vector_component($vx, 2), 3);
  $vs = [0, 0, 0, 0, 0, 1];
  assert_str('str_vector', vector_str_int($vs), '(0,0,0,0,0,1)');
  $vsize = [1, 2, 3, 4];
  assert_int('size', count($vsize), 4);
  $va = [1, 2, 3];
  $vb = [1, 1, 1];
  $vsum = vector_add($va, $vb);
  assert_int('add0', vector_component($vsum, 0), 2);
  assert_int('add1', vector_component($vsum, 1), 3);
  assert_int('add2', vector_component($vsum, 2), 4);
  $vsub = vector_sub($va, $vb);
  assert_int('sub0', vector_component($vsub, 0), 0);
  assert_int('sub1', vector_component($vsub, 1), 1);
  assert_int('sub2', vector_component($vsub, 2), 2);
  $vmul = vector_scalar_mul($va, 3.0);
  assert_str('scalar_mul', vector_str_float($vmul, 1), '(3.0,6.0,9.0)');
  assert_int('dot_product', vector_dot([2, -1, 4], [1, -2, -1]), 0);
  $zvec = zero_vector(10);
  $zstr = vector_str_int($zvec);
  $zcount = 0;
  $zi = 0;
  while ($zi < strlen($zstr)) {
  if (substr($zstr, $zi, $zi + 1 - $zi) == '0') {
  $zcount = $zcount + 1;
}
  $zi = $zi + 1;
}
  assert_int('zero_vector', $zcount, 10);
  assert_str('unit_basis', vector_str_int(unit_basis_vector(3, 1)), '(0,1,0)');
  assert_str('axpy', vector_str_int(axpy(2, [1, 2, 3], [1, 0, 1])), '(3,4,7)');
  $vcopy = copy_vector([1, 0, 0, 0, 0, 0]);
  assert_str('copy', vector_str_int($vcopy), '(1,0,0,0,0,0)');
  $vchange = [1, 0, 0];
  change_component($vchange, 0, 0);
  change_component($vchange, 1, 1);
  assert_str('change_component', vector_str_int($vchange), '(0,1,0)');
  $ma = [[1, 2, 3], [2, 4, 5], [6, 7, 8]];
  assert_str('matrix_str', matrix_str($ma), '|1,2,3|
|2,4,5|
|6,7,8|
');
  assert_int('determinant', determinant($ma), -5);
  $mb = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
  $mv = matrix_mul_vector($mb, [1, 2, 3]);
  assert_str('matrix_vec_mul', vector_str_int($mv), '(14,32,50)');
  $msc = matrix_mul_scalar($mb, 2);
  assert_str('matrix_scalar_mul', matrix_str($msc), '|2,4,6|
|8,10,12|
|14,16,18|
');
  $mc = [[1, 2, 3], [2, 4, 5], [6, 7, 8]];
  matrix_change_component($mc, 0, 2, 5);
  assert_str('change_component_matrix', matrix_str($mc), '|1,2,5|
|2,4,5|
|6,7,8|
');
  assert_int('matrix_component', matrix_component($mc, 2, 1), 7);
  $madd = matrix_add([[1, 2, 3], [2, 4, 5], [6, 7, 8]], [[1, 2, 7], [2, 4, 5], [6, 7, 10]]);
  assert_str('matrix_add', matrix_str($madd), '|2,4,10|
|4,8,10|
|12,14,18|
');
  $msub = matrix_sub([[1, 2, 3], [2, 4, 5], [6, 7, 8]], [[1, 2, 7], [2, 4, 5], [6, 7, 10]]);
  assert_str('matrix_sub', matrix_str($msub), '|0,0,-4|
|0,0,0|
|0,0,-2|
');
  $mzero = square_zero_matrix(5);
  assert_str('square_zero_matrix', matrix_str($mzero), '|0,0,0,0,0|
|0,0,0,0,0|
|0,0,0,0,0|
|0,0,0,0,0|
|0,0,0,0,0|
');
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
