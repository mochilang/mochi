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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function to_float($x) {
  return _imul($x, 1.0);
}
function mochi_sqrt($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = _iadd($i, 1);
};
  return $guess;
}
function mochi_floor($x) {
  $n = 0;
  $y = $x;
  while ($y >= 1.0) {
  $y = $y - 1.0;
  $n = _iadd($n, 1);
};
  return $n;
}
function hexagonal_num($n) {
  return _imul($n, (_isub(_imul(2, $n), 1)));
}
function is_pentagonal($n) {
  $root = mochi_sqrt(1.0 + 24.0 * floatval($n));
  $val = (1.0 + $root) / 6.0;
  return $val == floatval(mochi_floor($val));
}
function solution($start) {
  $idx = $start;
  $num = hexagonal_num($idx);
  while (!is_pentagonal($num)) {
  $idx = _iadd($idx, 1);
  $num = hexagonal_num($idx);
};
  return $num;
}
function test_hexagonal_num() {
  if (hexagonal_num(143) != 40755) {
  _panic('hexagonal_num(143) failed');
}
  if (hexagonal_num(21) != 861) {
  _panic('hexagonal_num(21) failed');
}
  if (hexagonal_num(10) != 190) {
  _panic('hexagonal_num(10) failed');
}
}
function test_is_pentagonal() {
  if (!is_pentagonal(330)) {
  _panic('330 should be pentagonal');
}
  if (is_pentagonal(7683)) {
  _panic('7683 should not be pentagonal');
}
  if (!is_pentagonal(2380)) {
  _panic('2380 should be pentagonal');
}
}
function test_solution() {
  if (solution(144) != 1533776805) {
  _panic('solution failed');
}
}
test_hexagonal_num();
test_is_pentagonal();
test_solution();
echo rtrim(_str(solution(144)) . ' = '), PHP_EOL;
