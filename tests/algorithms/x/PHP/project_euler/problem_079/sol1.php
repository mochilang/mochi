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
function parse_int($s) {
  global $logins1, $logins2;
  $value = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $value = _iadd(_imul($value, 10), (intval($c)));
  $i = _iadd($i, 1);
};
  return $value;
}
function mochi_join($xs) {
  global $logins1, $logins2;
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . $xs[$i];
  $i = _iadd($i, 1);
};
  return $s;
}
function contains($xs, $c) {
  global $logins1, $logins2;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $c) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function index_of($xs, $c) {
  global $logins1, $logins2;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $c) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function remove_at($xs, $idx) {
  global $logins1, $logins2;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function unique_chars($logins) {
  global $logins1, $logins2;
  $chars = [];
  $i = 0;
  while ($i < count($logins)) {
  $login = $logins[$i];
  $j = 0;
  while ($j < strlen($login)) {
  $c = substr($login, $j, $j + 1 - $j);
  if (!contains($chars, $c)) {
  $chars = _append($chars, $c);
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $chars;
}
function satisfies($permutation, $logins) {
  global $logins1, $logins2;
  $i = 0;
  while ($i < count($logins)) {
  $login = $logins[$i];
  $i0 = index_of($permutation, substr($login, 0, 0 + 1));
  $i1 = index_of($permutation, substr($login, 1, 1 + 1 - 1));
  $i2 = index_of($permutation, substr($login, 2, 2 + 1 - 2));
  if (!($i0 < $i1 && $i1 < $i2)) {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function search($chars, $current, $logins) {
  global $logins1, $logins2;
  if (count($chars) == 0) {
  if (satisfies($current, $logins)) {
  return mochi_join($current);
};
  return '';
}
  $i = 0;
  while ($i < count($chars)) {
  $c = $chars[$i];
  $rest = remove_at($chars, $i);
  $next = _append($current, $c);
  $res = search($rest, $next, $logins);
  if ($res != '') {
  return $res;
}
  $i = _iadd($i, 1);
};
  return '';
}
function find_secret_passcode($logins) {
  global $logins1, $logins2;
  $chars = unique_chars($logins);
  $s = search($chars, [], $logins);
  if ($s == '') {
  return -1;
}
  return parse_int($s);
}
$logins1 = ['135', '259', '235', '189', '690', '168', '120', '136', '289', '589', '160', '165', '580', '369', '250', '280'];
echo rtrim(_str(find_secret_passcode($logins1))), PHP_EOL;
$logins2 = ['426', '281', '061', '819', '268', '406', '420', '428', '209', '689', '019', '421', '469', '261', '681', '201'];
echo rtrim(_str(find_secret_passcode($logins2))), PHP_EOL;
