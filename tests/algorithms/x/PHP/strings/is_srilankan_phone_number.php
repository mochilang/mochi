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
function starts_with($s, $prefix) {
  global $phone;
  if (strlen($s) < strlen($prefix)) {
  return false;
}
  return substr($s, 0, strlen($prefix)) == $prefix;
}
function all_digits($s) {
  global $phone;
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  if ($c < '0' || $c > '9') {
  return false;
}
  $i = _iadd($i, 1);
};
  return true;
}
function is_sri_lankan_phone_number($phone) {
  $p = $phone;
  if (starts_with($p, '+94')) {
  $p = substr($p, 3, strlen($p) - 3);
} else {
  if (starts_with($p, '0094')) {
  $p = substr($p, 4, strlen($p) - 4);
} else {
  if (starts_with($p, '94')) {
  $p = substr($p, 2, strlen($p) - 2);
} else {
  if (starts_with($p, '0')) {
  $p = substr($p, 1, strlen($p) - 1);
} else {
  return false;
};
};
};
}
  if (strlen($p) != 9 && strlen($p) != 10) {
  return false;
}
  if (substr($p, 0, 0 + 1) != '7') {
  return false;
}
  $second = substr($p, 1, 1 + 1 - 1);
  $allowed = ['0', '1', '2', '4', '5', '6', '7', '8'];
  if (!(in_array($second, $allowed))) {
  return false;
}
  $idx = 2;
  if (strlen($p) == 10) {
  $sep = substr($p, 2, 2 + 1 - 2);
  if ($sep != '-' && $sep != ' ') {
  return false;
};
  $idx = 3;
}
  if (_isub(strlen($p), $idx) != 7) {
  return false;
}
  $rest = substr($p, $idx, strlen($p) - $idx);
  return all_digits($rest);
}
$phone = '0094702343221';
echo rtrim(_str(is_sri_lankan_phone_number($phone))), PHP_EOL;
