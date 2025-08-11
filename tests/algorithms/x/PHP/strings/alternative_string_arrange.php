<?php
ini_set('memory_limit', '-1');
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
function alternative_string_arrange($first_str, $second_str) {
  $len1 = strlen($first_str);
  $len2 = strlen($second_str);
  $res = '';
  $i = 0;
  while ($i < $len1 || $i < $len2) {
  if ($i < $len1) {
  $res = $res . substr($first_str, $i, $i + 1 - $i);
}
  if ($i < $len2) {
  $res = $res . substr($second_str, $i, $i + 1 - $i);
}
  $i = _iadd($i, 1);
};
  return $res;
}
echo rtrim(alternative_string_arrange('ABCD', 'XY')), PHP_EOL;
echo rtrim(alternative_string_arrange('XY', 'ABCD')), PHP_EOL;
echo rtrim(alternative_string_arrange('AB', 'XYZ')), PHP_EOL;
echo rtrim(alternative_string_arrange('ABC', '')), PHP_EOL;
