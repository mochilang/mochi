<?php
ini_set('memory_limit', '-1');
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
function fenwick_from_list($arr) {
  global $f_base, $f, $f2, $f3;
  $size = count($arr);
  $tree = [];
  $i = 0;
  while ($i < $size) {
  $tree = _append($tree, $arr[$i]);
  $i = $i + 1;
};
  $i = 1;
  while ($i < $size) {
  $j = fenwick_next($i);
  if ($j < $size) {
  $tree[$j] = $tree[$j] + $tree[$i];
}
  $i = $i + 1;
};
  return ['size' => $size, 'tree' => $tree];
}
function fenwick_empty($size) {
  global $f_base, $f, $f2, $f3;
  $tree = [];
  $i = 0;
  while ($i < $size) {
  $tree = _append($tree, 0);
  $i = $i + 1;
};
  return ['size' => $size, 'tree' => $tree];
}
function fenwick_get_array($f) {
  global $f_base, $f2, $f3;
  $arr = [];
  $i = 0;
  while ($i < $f['size']) {
  $arr = _append($arr, $f['tree'][$i]);
  $i = $i + 1;
};
  $i = $f['size'] - 1;
  while ($i > 0) {
  $j = fenwick_next($i);
  if ($j < $f['size']) {
  $arr[$j] = $arr[$j] - $arr[$i];
}
  $i = $i - 1;
};
  return $arr;
}
function bit_and($a, $b) {
  global $f_base, $f, $f2, $f3;
  $ua = $a;
  $ub = $b;
  $res = 0;
  $bit = 1;
  while ($ua != 0 || $ub != 0) {
  if ($ua % 2 == 1 && $ub % 2 == 1) {
  $res = $res + $bit;
}
  $ua = intval((_intdiv($ua, 2)));
  $ub = intval((_intdiv($ub, 2)));
  $bit = $bit * 2;
};
  return $res;
}
function low_bit($x) {
  global $f_base, $f, $f2, $f3;
  if ($x == 0) {
  return 0;
}
  return $x - bit_and($x, $x - 1);
}
function fenwick_next($index) {
  global $f_base, $f, $f2, $f3;
  return $index + low_bit($index);
}
function fenwick_prev($index) {
  global $f_base, $f, $f2, $f3;
  return $index - low_bit($index);
}
function fenwick_add($f, $index, $value) {
  global $f_base, $f2, $f3;
  $tree = $f['tree'];
  if ($index == 0) {
  $tree[0] = $tree[0] + $value;
  return ['size' => $f['size'], 'tree' => $tree];
}
  $i = $index;
  while ($i < $f['size']) {
  $tree[$i] = $tree[$i] + $value;
  $i = fenwick_next($i);
};
  return ['size' => $f['size'], 'tree' => $tree];
}
function fenwick_update($f, $index, $value) {
  global $f_base, $f2, $f3;
  $current = fenwick_get($f, $index);
  return fenwick_add($f, $index, $value - $current);
}
function fenwick_prefix($f, $right) {
  global $f_base, $f2, $f3;
  if ($right == 0) {
  return 0;
}
  $result = $f['tree'][0];
  $r = $right - 1;
  while ($r > 0) {
  $result = $result + $f['tree'][$r];
  $r = fenwick_prev($r);
};
  return $result;
}
function fenwick_query($f, $left, $right) {
  global $f_base, $f2, $f3;
  return fenwick_prefix($f, $right) - fenwick_prefix($f, $left);
}
function fenwick_get($f, $index) {
  global $f_base, $f2, $f3;
  return fenwick_query($f, $index, $index + 1);
}
function fenwick_rank_query($f, $value) {
  global $f_base, $f2, $f3;
  $v = $value - $f['tree'][0];
  if ($v < 0) {
  return -1;
}
  $j = 1;
  while ($j * 2 < $f['size']) {
  $j = $j * 2;
};
  $i = 0;
  $jj = $j;
  while ($jj > 0) {
  if ($i + $jj < $f['size'] && $f['tree'][$i + $jj] <= $v) {
  $v = $v - $f['tree'][$i + $jj];
  $i = $i + $jj;
}
  $jj = _intdiv($jj, 2);
};
  return $i;
}
$f_base = fenwick_from_list([1, 2, 3, 4, 5]);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(fenwick_get_array($f_base), 1344))))))), PHP_EOL;
$f = fenwick_from_list([1, 2, 3, 4, 5]);
$f = fenwick_add($f, 0, 1);
$f = fenwick_add($f, 1, 2);
$f = fenwick_add($f, 2, 3);
$f = fenwick_add($f, 3, 4);
$f = fenwick_add($f, 4, 5);
echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(fenwick_get_array($f), 1344))))))), PHP_EOL;
$f2 = fenwick_from_list([1, 2, 3, 4, 5]);
echo rtrim(json_encode(fenwick_prefix($f2, 3), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_query($f2, 1, 4), 1344)), PHP_EOL;
$f3 = fenwick_from_list([1, 2, 0, 3, 0, 5]);
echo rtrim(json_encode(fenwick_rank_query($f3, 0), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 2), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 1), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 3), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 5), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 6), 1344)), PHP_EOL;
echo rtrim(json_encode(fenwick_rank_query($f3, 11), 1344)), PHP_EOL;
