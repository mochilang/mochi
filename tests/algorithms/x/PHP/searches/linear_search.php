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
function linear_search($sequence, $target) {
  $i = 0;
  while ($i < count($sequence)) {
  if ($sequence[$i] == $target) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function rec_linear_search($sequence, $low, $high, $target) {
  if (!(0 <= $high && $high < count($sequence) && 0 <= $low && $low < count($sequence))) {
  _panic('Invalid upper or lower bound!');
}
  if ($high < $low) {
  return -1;
}
  if ($sequence[$low] == $target) {
  return $low;
}
  if ($sequence[$high] == $target) {
  return $high;
}
  return rec_linear_search($sequence, _iadd($low, 1), _isub($high, 1), $target);
}
echo rtrim(_str(linear_search([0, 5, 7, 10, 15], 0))), PHP_EOL;
echo rtrim(_str(linear_search([0, 5, 7, 10, 15], 15))), PHP_EOL;
echo rtrim(_str(linear_search([0, 5, 7, 10, 15], 5))), PHP_EOL;
echo rtrim(_str(linear_search([0, 5, 7, 10, 15], 6))), PHP_EOL;
echo rtrim(_str(rec_linear_search([0, 30, 500, 100, 700], 0, 4, 0))), PHP_EOL;
echo rtrim(_str(rec_linear_search([0, 30, 500, 100, 700], 0, 4, 700))), PHP_EOL;
echo rtrim(_str(rec_linear_search([0, 30, 500, 100, 700], 0, 4, 30))), PHP_EOL;
echo rtrim(_str(rec_linear_search([0, 30, 500, 100, 700], 0, 4, -6))), PHP_EOL;
