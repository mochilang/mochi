<?php
ini_set('memory_limit', '-1');
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
function strip_and_remove_spaces($s) {
  $start = 0;
  $end = _isub(strlen($s), 1);
  while ($start < strlen($s) && substr($s, $start, $start + 1 - $start) == ' ') {
  $start = _iadd($start, 1);
};
  while ($end >= $start && substr($s, $end, $end + 1 - $end) == ' ') {
  $end = _isub($end, 1);
};
  $res = '';
  $i = $start;
  while ($i <= $end) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch != ' ') {
  $res = $res . $ch;
}
  $i = _iadd($i, 1);
};
  return $res;
}
function check_anagrams($a, $b) {
  $s1 = strtolower($a);
  $s2 = strtolower($b);
  $s1 = strip_and_remove_spaces($s1);
  $s2 = strip_and_remove_spaces($s2);
  if (strlen($s1) != strlen($s2)) {
  return false;
}
  $count = [];
  $i = 0;
  while ($i < strlen($s1)) {
  $c1 = substr($s1, $i, $i + 1 - $i);
  $c2 = substr($s2, $i, $i + 1 - $i);
  if (array_key_exists($c1, $count)) {
  $count[$c1] = _iadd($count[$c1], 1);
} else {
  $count[$c1] = 1;
}
  if (array_key_exists($c2, $count)) {
  $count[$c2] = _isub($count[$c2], 1);
} else {
  $count[$c2] = -1;
}
  $i = _iadd($i, 1);
};
  foreach (array_keys($count) as $ch) {
  if ($count[$ch] != 0) {
  return false;
}
};
  return true;
}
function print_bool($b) {
  if ($b) {
  echo rtrim((true ? 'true' : 'false')), PHP_EOL;
} else {
  echo rtrim((false ? 'true' : 'false')), PHP_EOL;
}
}
print_bool(check_anagrams('Silent', 'Listen'));
print_bool(check_anagrams('This is a string', 'Is this a string'));
print_bool(check_anagrams('This is    a      string', 'Is     this a string'));
print_bool(check_anagrams('There', 'Their'));
