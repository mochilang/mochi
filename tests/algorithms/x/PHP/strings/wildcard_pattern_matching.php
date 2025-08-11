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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function make_matrix_bool($rows, $cols, $init) {
  $matrix = [];
  for ($_ = 0; $_ < $rows; $_++) {
  $row = [];
  for ($_2 = 0; $_2 < $cols; $_2++) {
  $row = _append($row, $init);
};
  $matrix = _append($matrix, $row);
};
  return $matrix;
}
function match_pattern($input_string, $pattern) {
  $len_string = _iadd(strlen($input_string), 1);
  $len_pattern = _iadd(strlen($pattern), 1);
  $dp = make_matrix_bool($len_string, $len_pattern, false);
  $row0 = $dp[0];
  $row0[0] = true;
  $dp[0] = $row0;
  $j = 1;
  while ($j < $len_pattern) {
  $row0 = $dp[0];
  if (substr($pattern, _isub($j, 1), $j - _isub($j, 1)) == '*') {
  $row0[$j] = $row0[_isub($j, 2)];
} else {
  $row0[$j] = false;
}
  $dp[0] = $row0;
  $j = _iadd($j, 1);
};
  $i = 1;
  while ($i < $len_string) {
  $row = $dp[$i];
  $j2 = 1;
  while ($j2 < $len_pattern) {
  $s_char = substr($input_string, _isub($i, 1), $i - _isub($i, 1));
  $p_char = substr($pattern, _isub($j2, 1), $j2 - _isub($j2, 1));
  if ($s_char == $p_char || $p_char == '.') {
  $row[$j2] = $dp[_isub($i, 1)][_isub($j2, 1)];
} else {
  if ($p_char == '*') {
  $val = $dp[$i][_isub($j2, 2)];
  $prev_p = substr($pattern, _isub($j2, 2), _isub($j2, 1) - _isub($j2, 2));
  if (!$val && ($prev_p == $s_char || $prev_p == '.')) {
  $val = $dp[_isub($i, 1)][$j2];
};
  $row[$j2] = $val;
} else {
  $row[$j2] = false;
};
}
  $j2 = _iadd($j2, 1);
};
  $dp[$i] = $row;
  $i = _iadd($i, 1);
};
  return $dp[_isub($len_string, 1)][_isub($len_pattern, 1)];
}
function main() {
  if (!match_pattern('aab', 'c*a*b')) {
  _panic('case1 failed');
}
  if (match_pattern('dabc', '*abc')) {
  _panic('case2 failed');
}
  if (match_pattern('aaa', 'aa')) {
  _panic('case3 failed');
}
  if (!match_pattern('aaa', 'a.a')) {
  _panic('case4 failed');
}
  if (match_pattern('aaab', 'aa*')) {
  _panic('case5 failed');
}
  if (!match_pattern('aaab', '.*')) {
  _panic('case6 failed');
}
  if (match_pattern('a', 'bbbb')) {
  _panic('case7 failed');
}
  if (match_pattern('', 'bbbb')) {
  _panic('case8 failed');
}
  if (match_pattern('a', '')) {
  _panic('case9 failed');
}
  if (!match_pattern('', '')) {
  _panic('case10 failed');
}
  echo rtrim(_str(match_pattern('aab', 'c*a*b'))), PHP_EOL;
  echo rtrim(_str(match_pattern('dabc', '*abc'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaa', 'aa'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaa', 'a.a'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaab', 'aa*'))), PHP_EOL;
  echo rtrim(_str(match_pattern('aaab', '.*'))), PHP_EOL;
  echo rtrim(_str(match_pattern('a', 'bbbb'))), PHP_EOL;
  echo rtrim(_str(match_pattern('', 'bbbb'))), PHP_EOL;
  echo rtrim(_str(match_pattern('a', ''))), PHP_EOL;
  echo rtrim(_str(match_pattern('', ''))), PHP_EOL;
}
main();
