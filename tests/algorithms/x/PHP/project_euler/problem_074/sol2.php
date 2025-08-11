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
$DIGIT_FACTORIAL = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880];
function digit_factorial_sum($number) {
  global $DIGIT_FACTORIAL;
  if ($number < 0) {
  _panic('Parameter number must be greater than or equal to 0');
}
  if ($number == 0) {
  return $DIGIT_FACTORIAL[0];
}
  $n = $number;
  $total = 0;
  while ($n > 0) {
  $digit = _imod($n, 10);
  $total = _iadd($total, $DIGIT_FACTORIAL[$digit]);
  $n = _intdiv($n, 10);
};
  return $total;
}
function chain_len($n, $limit) {
  global $DIGIT_FACTORIAL;
  $seen = [];
  $length = 0;
  $cur = $n;
  while ((array_key_exists($cur, $seen)) == false && $length <= $limit) {
  $seen[$cur] = true;
  $length = _iadd($length, 1);
  $cur = digit_factorial_sum($cur);
};
  return $length;
}
function solution($chain_length, $number_limit) {
  global $DIGIT_FACTORIAL;
  if ($chain_length <= 0 || $number_limit <= 0) {
  _panic('Parameters chain_length and number_limit must be greater than 0');
}
  $count = 0;
  $start = 1;
  while ($start < $number_limit) {
  if (chain_len($start, $chain_length) == $chain_length) {
  $count = _iadd($count, 1);
}
  $start = _iadd($start, 1);
};
  return $count;
}
echo rtrim(_str(solution(60, 1000000))), PHP_EOL;
