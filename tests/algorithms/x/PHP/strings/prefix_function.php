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
function prefix_function($s) {
  $pi = [];
  $i = 0;
  while ($i < strlen($s)) {
  $pi = _append($pi, 0);
  $i = _iadd($i, 1);
};
  $i = 1;
  while ($i < strlen($s)) {
  $j = $pi[_isub($i, 1)];
  while ($j > 0 && substr($s, $i, $i + 1 - $i) != substr($s, $j, $j + 1 - $j)) {
  $j = $pi[_isub($j, 1)];
};
  if (substr($s, $i, $i + 1 - $i) == substr($s, $j, $j + 1 - $j)) {
  $j = _iadd($j, 1);
}
  $pi[$i] = $j;
  $i = _iadd($i, 1);
};
  return $pi;
}
function longest_prefix($s) {
  $pi = prefix_function($s);
  $max_val = 0;
  $i = 0;
  while ($i < count($pi)) {
  if ($pi[$i] > $max_val) {
  $max_val = $pi[$i];
}
  $i = _iadd($i, 1);
};
  return $max_val;
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
function test_prefix_function() {
  $s1 = 'aabcdaabc';
  $expected1 = [0, 1, 0, 0, 0, 1, 2, 3, 4];
  $r1 = prefix_function($s1);
  if (!list_eq_int($r1, $expected1)) {
  _panic('prefix_function aabcdaabc failed');
}
  $s2 = 'asdasdad';
  $expected2 = [0, 0, 0, 1, 2, 3, 4, 0];
  $r2 = prefix_function($s2);
  if (!list_eq_int($r2, $expected2)) {
  _panic('prefix_function asdasdad failed');
}
}
function test_longest_prefix() {
  if (longest_prefix('aabcdaabc') != 4) {
  _panic('longest_prefix example1 failed');
}
  if (longest_prefix('asdasdad') != 4) {
  _panic('longest_prefix example2 failed');
}
  if (longest_prefix('abcab') != 2) {
  _panic('longest_prefix example3 failed');
}
}
function main() {
  test_prefix_function();
  test_longest_prefix();
  $r1 = prefix_function('aabcdaabc');
  $r2 = prefix_function('asdasdad');
  echo rtrim(_str($r1)), PHP_EOL;
  echo rtrim(_str($r2)), PHP_EOL;
  echo rtrim(_str(longest_prefix('aabcdaabc'))), PHP_EOL;
  echo rtrim(_str(longest_prefix('abcab'))), PHP_EOL;
}
main();
