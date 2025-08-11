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
  function max_val($arr) {
  global $ascii_chars, $example1, $example2, $example3;
  $m = $arr[0];
  $i = 1;
  while ($i < count($arr)) {
  if ($arr[$i] > $m) {
  $m = $arr[$i];
}
  $i = _iadd($i, 1);
};
  return $m;
};
  function min_val($arr) {
  global $ascii_chars, $example1, $example2, $example3;
  $m = $arr[0];
  $i = 1;
  while ($i < count($arr)) {
  if ($arr[$i] < $m) {
  $m = $arr[$i];
}
  $i = _iadd($i, 1);
};
  return $m;
};
  function counting_sort($collection) {
  global $ascii_chars, $example1, $example2, $example3;
  if (count($collection) == 0) {
  return [];
}
  $coll_len = count($collection);
  $coll_max = max_val($collection);
  $coll_min = min_val($collection);
  $counting_arr_length = _isub(_iadd($coll_max, 1), $coll_min);
  $counting_arr = [];
  $i = 0;
  while ($i < $counting_arr_length) {
  $counting_arr = _append($counting_arr, 0);
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < $coll_len) {
  $number = $collection[$i];
  $counting_arr[_isub($number, $coll_min)] = _iadd($counting_arr[_isub($number, $coll_min)], 1);
  $i = _iadd($i, 1);
};
  $i = 1;
  while ($i < $counting_arr_length) {
  $counting_arr[$i] = _iadd($counting_arr[$i], $counting_arr[_isub($i, 1)]);
  $i = _iadd($i, 1);
};
  $ordered = [];
  $i = 0;
  while ($i < $coll_len) {
  $ordered = _append($ordered, 0);
  $i = _iadd($i, 1);
};
  $idx = _isub($coll_len, 1);
  while ($idx >= 0) {
  $number = $collection[$idx];
  $pos = _isub($counting_arr[_isub($number, $coll_min)], 1);
  $ordered[$pos] = $number;
  $counting_arr[_isub($number, $coll_min)] = _isub($counting_arr[_isub($number, $coll_min)], 1);
  $idx = _isub($idx, 1);
};
  return $ordered;
};
  $ascii_chars = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
  function mochi_chr($code) {
  global $ascii_chars, $example1, $example2, $example3;
  if ($code == 10) {
  return '
';
}
  if ($code == 13) {
  return '';
}
  if ($code == 9) {
  return '	';
}
  if ($code >= 32 && $code < 127) {
  return substr($ascii_chars, _isub($code, 32), _isub($code, 31) - _isub($code, 32));
}
  return '';
};
  function mochi_ord($ch) {
  global $ascii_chars, $example1, $example2, $example3;
  if ($ch == '
') {
  return 10;
}
  if ($ch == '') {
  return 13;
}
  if ($ch == '	') {
  return 9;
}
  $i = 0;
  while ($i < strlen($ascii_chars)) {
  if (substr($ascii_chars, $i, _iadd($i, 1) - $i) == $ch) {
  return _iadd(32, $i);
}
  $i = _iadd($i, 1);
};
  return 0;
};
  function counting_sort_string($s) {
  global $ascii_chars, $example1, $example2, $example3;
  $codes = [];
  $i = 0;
  while ($i < strlen($s)) {
  $codes = _append($codes, mochi_ord($s[$i]));
  $i = _iadd($i, 1);
};
  $sorted_codes = counting_sort($codes);
  $res = '';
  $i = 0;
  while ($i < count($sorted_codes)) {
  $res = $res . mochi_chr($sorted_codes[$i]);
  $i = _iadd($i, 1);
};
  return $res;
};
  $example1 = counting_sort([0, 5, 3, 2, 2]);
  echo rtrim(_str($example1)), PHP_EOL;
  $example2 = counting_sort([]);
  echo rtrim(_str($example2)), PHP_EOL;
  $example3 = counting_sort([-2, -5, -45]);
  echo rtrim(_str($example3)), PHP_EOL;
  echo rtrim(counting_sort_string('thisisthestring')), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
