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
function _sha256($bs) {
    $bin = '';
    foreach ($bs as $b) { $bin .= chr($b); }
    $hash = hash('sha256', $bin, true);
    return array_values(unpack('C*', $hash));
}
$HEX = '0123456789abcdef';
function byte_to_hex($b) {
  global $HEX, $expected, $answer, $computed;
  $hi = _intdiv($b, 16);
  $lo = _imod($b, 16);
  return substr($HEX, $hi, $hi + 1 - $hi) . substr($HEX, $lo, $lo + 1 - $lo);
}
function bytes_to_hex($bs) {
  global $HEX, $expected, $answer, $computed;
  $res = '';
  $i = 0;
  while ($i < count($bs)) {
  $res = $res . byte_to_hex($bs[$i]);
  $i = _iadd($i, 1);
};
  return $res;
}
function sha256_hex($s) {
  global $HEX, $expected, $answer, $computed;
  return bytes_to_hex(_sha256($s));
}
function solution_001() {
  global $HEX, $expected, $answer, $computed;
  $total = 0;
  $n = 0;
  while ($n < 1000) {
  if (_imod($n, 3) == 0 || _imod($n, 5) == 0) {
  $total = _iadd($total, $n);
}
  $n = _iadd($n, 1);
};
  return _str($total);
}
$expected = sha256_hex('233168');
$answer = solution_001();
$computed = sha256_hex($answer);
if ($computed == $expected) {
  echo rtrim('Problem 001 passed'), PHP_EOL;
} else {
  echo rtrim('Problem 001 failed: ' . $computed . ' != ' . $expected), PHP_EOL;
}
