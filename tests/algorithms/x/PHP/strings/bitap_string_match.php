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
function pow2($n) {
  $res = 1;
  $i = 0;
  while ($i < $n) {
  $res = _imul($res, 2);
  $i = _iadd($i, 1);
};
  return $res;
}
function bit_and($a, $b) {
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  while ($x > 0 || $y > 0) {
  if (_imod($x, 2) == 1 && _imod($y, 2) == 1) {
  $res = _iadd($res, $bit);
}
  $x = intval((_intdiv($x, 2)));
  $y = intval((_intdiv($y, 2)));
  $bit = _imul($bit, 2);
};
  return $res;
}
function bit_or($a, $b) {
  $x = $a;
  $y = $b;
  $res = 0;
  $bit = 1;
  while ($x > 0 || $y > 0) {
  if (_imod($x, 2) == 1 || _imod($y, 2) == 1) {
  $res = _iadd($res, $bit);
}
  $x = intval((_intdiv($x, 2)));
  $y = intval((_intdiv($y, 2)));
  $bit = _imul($bit, 2);
};
  return $res;
}
function char_to_index($ch) {
  $letters = 'abcdefghijklmnopqrstuvwxyz';
  $i = 0;
  while ($i < strlen($letters)) {
  if (substr($letters, $i, _iadd($i, 1) - $i) == $ch) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return 26;
}
function bitap_string_match($text, $pattern) {
  if ($pattern == '') {
  return 0;
}
  $m = strlen($pattern);
  if ($m > strlen($text)) {
  return -1;
}
  $limit = pow2(_iadd($m, 1));
  $all_ones = _isub($limit, 1);
  $pattern_mask = [];
  $i = 0;
  while ($i < 27) {
  $pattern_mask = _append($pattern_mask, $all_ones);
  $i = _iadd($i, 1);
};
  $i = 0;
  while ($i < $m) {
  $ch = substr($pattern, $i, _iadd($i, 1) - $i);
  $idx = char_to_index($ch);
  $pattern_mask[$idx] = bit_and($pattern_mask[$idx], _isub($all_ones, pow2($i)));
  $i = _iadd($i, 1);
};
  $state = _isub($all_ones, 1);
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, _iadd($i, 1) - $i);
  $idx = char_to_index($ch);
  $state = bit_or($state, $pattern_mask[$idx]);
  $state = _imod((_imul($state, 2)), $limit);
  if (bit_and($state, pow2($m)) == 0) {
  return _iadd(_isub($i, $m), 1);
}
  $i = _iadd($i, 1);
};
  return -1;
}
function main() {
  echo rtrim(_str(bitap_string_match('abdabababc', 'ababc'))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', ''))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', 'c'))), PHP_EOL;
  echo rtrim(_str(bitap_string_match('abdabababc', 'fofosdfo'))), PHP_EOL;
}
main();
