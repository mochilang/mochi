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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
function contains($chars, $ch) {
  $i = 0;
  while ($i < strlen($chars)) {
  if (substr($chars, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function substring($s, $start, $end) {
  $res = '';
  $i = $start;
  while ($i < $end) {
  $res = $res . substr($s, $i, $i + 1 - $i);
  $i = _iadd($i, 1);
};
  return $res;
}
function strip_chars($user_string, $characters) {
  $start = 0;
  $end = strlen($user_string);
  while ($start < $end && contains($characters, substr($user_string, $start, $start + 1 - $start))) {
  $start = _iadd($start, 1);
};
  while ($end > $start && contains($characters, substr($user_string, _isub($end, 1), _isub($end, 1) + 1 - _isub($end, 1)))) {
  $end = _isub($end, 1);
};
  return substr($user_string, $start, $end - $start);
}
function strip($user_string) {
  return strip_chars($user_string, ' 	
');
}
function test_strip() {
  if (strip('   hello   ') != 'hello') {
  _panic('test1 failed');
}
  if (strip_chars('...world...', '.') != 'world') {
  _panic('test2 failed');
}
  if (strip_chars('123hello123', '123') != 'hello') {
  _panic('test3 failed');
}
  if (strip('') != '') {
  _panic('test4 failed');
}
}
function main() {
  test_strip();
  echo rtrim(strip('   hello   ')), PHP_EOL;
  echo rtrim(strip_chars('...world...', '.')), PHP_EOL;
  echo rtrim(strip_chars('123hello123', '123')), PHP_EOL;
  echo rtrim(strip('')), PHP_EOL;
}
main();
