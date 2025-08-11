<?php
ini_set('memory_limit', '-1');
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
function get_failure_array($pattern) {
  global $text;
  $failure = [0];
  $i = 0;
  $j = 1;
  while ($j < strlen($pattern)) {
  if (substr($pattern, $i, _iadd($i, 1) - $i) == substr($pattern, $j, _iadd($j, 1) - $j)) {
  $i = _iadd($i, 1);
} else {
  if ($i > 0) {
  $i = $failure[_isub($i, 1)];
  continue;
};
}
  $j = _iadd($j, 1);
  $failure = _append($failure, $i);
};
  return $failure;
}
function knuth_morris_pratt($text, $pattern) {
  $failure = get_failure_array($pattern);
  $i = 0;
  $j = 0;
  while ($i < strlen($text)) {
  if (substr($pattern, $j, _iadd($j, 1) - $j) == substr($text, $i, _iadd($i, 1) - $i)) {
  if ($j == _isub(strlen($pattern), 1)) {
  return _isub($i, $j);
};
  $j = _iadd($j, 1);
} else {
  if ($j > 0) {
  $j = $failure[_isub($j, 1)];
  continue;
};
}
  $i = _iadd($i, 1);
};
  return -1;
}
$text = 'abcxabcdabxabcdabcdabcy';
$pattern = 'abcdabcy';
echo rtrim(json_encode(knuth_morris_pratt($text, $pattern), 1344)), PHP_EOL;
