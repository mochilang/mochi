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
function is_pangram($input_str) {
  global $s1, $s2;
  $letters = [];
  $i = 0;
  while ($i < strlen($input_str)) {
  $c = strtolower(substr($input_str, $i, $i + 1 - $i));
  $is_new = !(in_array($c, $letters));
  if ($c != ' ' && 'a' <= $c && $c <= 'z' && $is_new) {
  $letters = _append($letters, $c);
}
  $i = _iadd($i, 1);
};
  return count($letters) == 26;
}
function is_pangram_faster($input_str) {
  global $s1, $s2;
  $alphabet = 'abcdefghijklmnopqrstuvwxyz';
  $flag = [];
  $i = 0;
  while ($i < 26) {
  $flag = _append($flag, false);
  $i = _iadd($i, 1);
};
  $j = 0;
  while ($j < strlen($input_str)) {
  $c = strtolower(substr($input_str, $j, $j + 1 - $j));
  $k = 0;
  while ($k < 26) {
  if (substr($alphabet, $k, $k + 1 - $k) == $c) {
  $flag[$k] = true;
  break;
}
  $k = _iadd($k, 1);
};
  $j = _iadd($j, 1);
};
  $t = 0;
  while ($t < 26) {
  if (!$flag[$t]) {
  return false;
}
  $t = _iadd($t, 1);
};
  return true;
}
function is_pangram_fastest($input_str) {
  global $s1, $s2;
  $s = strtolower($input_str);
  $alphabet = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($alphabet)) {
  $letter = substr($alphabet, $i, $i + 1 - $i);
  if (!(strpos($s, $letter) !== false)) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
$s1 = 'The quick brown fox jumps over the lazy dog';
$s2 = 'My name is Unknown';
echo rtrim(_str(is_pangram($s1))), PHP_EOL;
echo rtrim(_str(is_pangram($s2))), PHP_EOL;
echo rtrim(_str(is_pangram_faster($s1))), PHP_EOL;
echo rtrim(_str(is_pangram_faster($s2))), PHP_EOL;
echo rtrim(_str(is_pangram_fastest($s1))), PHP_EOL;
echo rtrim(_str(is_pangram_fastest($s2))), PHP_EOL;
