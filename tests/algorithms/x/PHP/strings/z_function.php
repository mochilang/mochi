<?php
ini_set('memory_limit', '-1');
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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function z_function($s) {
  $z = [];
  $i = 0;
  while ($i < strlen($s)) {
  $z = _append($z, 0);
  $i = _iadd($i, 1);
};
  $l = 0;
  $r = 0;
  $i = 1;
  while ($i < strlen($s)) {
  if ($i <= $r) {
  $min_edge = _iadd(_isub($r, $i), 1);
  $zi = $z[_isub($i, $l)];
  if ($zi < $min_edge) {
  $min_edge = $zi;
};
  $z[$i] = $min_edge;
}
  while (go_next($i, $z, $s)) {
  $z[$i] = _iadd($z[$i], 1);
};
  if (_isub(_iadd($i, $z[$i]), 1) > $r) {
  $l = $i;
  $r = _isub(_iadd($i, $z[$i]), 1);
}
  $i = _iadd($i, 1);
};
  return $z;
}
function go_next($i, $z, $s) {
  return _iadd($i, $z[$i]) < strlen($s) && substr($s, $z[$i], $z[$i] + 1 - $z[$i]) == substr($s, _iadd($i, $z[$i]), _iadd($i, $z[$i]) + 1 - _iadd($i, $z[$i]));
}
function find_pattern($pattern, $input_str) {
  $answer = 0;
  $z_res = z_function($pattern . $input_str);
  $i = 0;
  while ($i < count($z_res)) {
  if ($z_res[$i] >= strlen($pattern)) {
  $answer = _iadd($answer, 1);
}
  $i = _iadd($i, 1);
};
  return $answer;
}
function list_eq_int($a, $b) {
  if (count($a) != count($b)) {
  return false;
}
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function test_z_function() {
  $s1 = 'abracadabra';
  $expected1 = [0, 0, 0, 1, 0, 1, 0, 4, 0, 0, 1];
  $r1 = z_function($s1);
  if (!list_eq_int($r1, $expected1)) {
  _panic('z_function abracadabra failed');
}
  $s2 = 'aaaa';
  $expected2 = [0, 3, 2, 1];
  $r2 = z_function($s2);
  if (!list_eq_int($r2, $expected2)) {
  _panic('z_function aaaa failed');
}
  $s3 = 'zxxzxxz';
  $expected3 = [0, 0, 0, 4, 0, 0, 1];
  $r3 = z_function($s3);
  if (!list_eq_int($r3, $expected3)) {
  _panic('z_function zxxzxxz failed');
}
}
function test_find_pattern() {
  if (find_pattern('abr', 'abracadabra') != 2) {
  _panic('find_pattern abr failed');
}
  if (find_pattern('a', 'aaaa') != 4) {
  _panic('find_pattern aaaa failed');
}
  if (find_pattern('xz', 'zxxzxxz') != 2) {
  _panic('find_pattern xz failed');
}
}
function main() {
  test_z_function();
  test_find_pattern();
  $r1 = z_function('abracadabra');
  $r2 = z_function('aaaa');
  $r3 = z_function('zxxzxxz');
  echo rtrim(_str($r1)), PHP_EOL;
  echo rtrim(_str($r2)), PHP_EOL;
  echo rtrim(_str($r3)), PHP_EOL;
  echo rtrim(_str(find_pattern('abr', 'abracadabra'))), PHP_EOL;
  echo rtrim(_str(find_pattern('a', 'aaaa'))), PHP_EOL;
  echo rtrim(_str(find_pattern('xz', 'zxxzxxz'))), PHP_EOL;
}
main();
