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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
function reverse($s) {
  global $test_data;
  $res = '';
  $i = _isub(strlen($s), 1);
  while ($i >= 0) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = _isub($i, 1);
};
  return $res;
}
function is_palindrome($s) {
  global $test_data;
  $start_i = 0;
  $end_i = _isub(strlen($s), 1);
  while ($start_i < $end_i) {
  if (substr($s, $start_i, $start_i + 1 - $start_i) == substr($s, $end_i, $end_i + 1 - $end_i)) {
  $start_i = _iadd($start_i, 1);
  $end_i = _isub($end_i, 1);
} else {
  return false;
}
};
  return true;
}
function is_palindrome_traversal($s) {
  global $test_data;
  $end = _idiv(strlen($s), 2);
  $n = strlen($s);
  $i = 0;
  while ($i < $end) {
  if (substr($s, $i, $i + 1 - $i) != substr($s, _isub(_isub($n, $i), 1), _isub(_isub($n, $i), 1) + 1 - _isub(_isub($n, $i), 1))) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function is_palindrome_recursive($s) {
  global $test_data;
  if (strlen($s) <= 1) {
  return true;
}
  if (substr($s, 0, 0 + 1) == substr($s, _isub(strlen($s), 1), _isub(strlen($s), 1) + 1 - _isub(strlen($s), 1))) {
  return is_palindrome_recursive(substr($s, 1, _isub(strlen($s), 1) - 1));
}
  return false;
}
function is_palindrome_slice($s) {
  global $test_data;
  return $s == reverse($s);
}
$test_data = [['text' => 'MALAYALAM', 'expected' => true], ['text' => 'String', 'expected' => false], ['text' => 'rotor', 'expected' => true], ['text' => 'level', 'expected' => true], ['text' => 'A', 'expected' => true], ['text' => 'BB', 'expected' => true], ['text' => 'ABC', 'expected' => false], ['text' => 'amanaplanacanalpanama', 'expected' => true]];
function main() {
  global $test_data;
  foreach ($test_data as $t) {
  $s = $t['text'];
  $expected = $t['expected'];
  $r1 = is_palindrome($s);
  $r2 = is_palindrome_traversal($s);
  $r3 = is_palindrome_recursive($s);
  $r4 = is_palindrome_slice($s);
  if ($r1 != $expected || $r2 != $expected || $r3 != $expected || $r4 != $expected) {
  _panic('algorithm mismatch');
}
  echo rtrim($s . ' ' . _str($expected)), PHP_EOL;
};
  echo rtrim('a man a plan a canal panama'), PHP_EOL;
}
main();
