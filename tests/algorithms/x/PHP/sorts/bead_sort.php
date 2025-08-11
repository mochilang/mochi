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
function bead_sort($sequence) {
  $n = count($sequence);
  $i = 0;
  while ($i < $n) {
  if ($sequence[$i] < 0) {
  _panic('Sequence must be list of non-negative integers');
}
  $i = _iadd($i, 1);
};
  $pass = 0;
  while ($pass < $n) {
  $j = 0;
  while ($j < _isub($n, 1)) {
  $upper = $sequence[$j];
  $lower = $sequence[_iadd($j, 1)];
  if ($upper > $lower) {
  $diff = _isub($upper, $lower);
  $sequence[$j] = _isub($upper, $diff);
  $sequence[_iadd($j, 1)] = _iadd($lower, $diff);
}
  $j = _iadd($j, 1);
};
  $pass = _iadd($pass, 1);
};
  return $sequence;
}
echo rtrim(_str(bead_sort([6, 11, 12, 4, 1, 5]))), PHP_EOL;
echo rtrim(_str(bead_sort([9, 8, 7, 6, 5, 4, 3, 2, 1]))), PHP_EOL;
echo rtrim(_str(bead_sort([5, 0, 4, 3]))), PHP_EOL;
echo rtrim(_str(bead_sort([8, 2, 1]))), PHP_EOL;
