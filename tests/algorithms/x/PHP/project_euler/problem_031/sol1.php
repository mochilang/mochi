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
function one_pence() {
  return 1;
}
function two_pence($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(two_pence(_isub($x, 2)), one_pence());
}
function five_pence($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(five_pence(_isub($x, 5)), two_pence($x));
}
function ten_pence($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(ten_pence(_isub($x, 10)), five_pence($x));
}
function twenty_pence($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(twenty_pence(_isub($x, 20)), ten_pence($x));
}
function fifty_pence($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(fifty_pence(_isub($x, 50)), twenty_pence($x));
}
function one_pound($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(one_pound(_isub($x, 100)), fifty_pence($x));
}
function two_pound($x) {
  if ($x < 0) {
  return 0;
}
  return _iadd(two_pound(_isub($x, 200)), one_pound($x));
}
function solution($n) {
  return two_pound($n);
}
function main() {
  $n = intval(trim(fgets(STDIN)));
  echo rtrim(_str(solution($n))), PHP_EOL;
}
main();
