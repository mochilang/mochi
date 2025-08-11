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
function mochi_join($separator, $separated) {
  $joined = '';
  $last_index = _isub(count($separated), 1);
  $i = 0;
  while ($i < count($separated)) {
  $joined = $joined . $separated[$i];
  if ($i < $last_index) {
  $joined = $joined . $separator;
}
  $i = _iadd($i, 1);
};
  return $joined;
}
function main() {
  echo rtrim(json_encode(mochi_join('', ['a', 'b', 'c', 'd']), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join('#', ['a', 'b', 'c', 'd']), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join('#', ['a']), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join(' ', ['You', 'are', 'amazing!']), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join(',', ['', '', '']), 1344)), PHP_EOL;
  echo rtrim(json_encode(mochi_join('-', ['apple', 'banana', 'cherry']), 1344)), PHP_EOL;
}
main();
