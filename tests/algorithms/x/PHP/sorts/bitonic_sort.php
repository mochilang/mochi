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
function set_at_int($xs, $idx, $value) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = _iadd($i, 1);
};
  return $res;
}
function comp_and_swap($arr, $i, $j, $dir) {
  $res = $arr;
  $xi = $arr[$i];
  $xj = $arr[$j];
  if (($dir == 1 && $xi > $xj) || ($dir == 0 && $xi < $xj)) {
  $res = set_at_int($res, $i, $xj);
  $res = set_at_int($res, $j, $xi);
}
  return $res;
}
function bitonic_merge($arr, $low, $length, $dir) {
  $res = $arr;
  if ($length > 1) {
  $mid = _intdiv($length, 2);
  $k = $low;
  while ($k < _iadd($low, $mid)) {
  $res = comp_and_swap($res, $k, _iadd($k, $mid), $dir);
  $k = _iadd($k, 1);
};
  $res = bitonic_merge($res, $low, $mid, $dir);
  $res = bitonic_merge($res, _iadd($low, $mid), $mid, $dir);
}
  return $res;
}
function bitonic_sort($arr, $low, $length, $dir) {
  $res = $arr;
  if ($length > 1) {
  $mid = _intdiv($length, 2);
  $res = bitonic_sort($res, $low, $mid, 1);
  $res = bitonic_sort($res, _iadd($low, $mid), $mid, 0);
  $res = bitonic_merge($res, $low, $length, $dir);
}
  return $res;
}
function main() {
  $data = [12, 34, 92, -23, 0, -121, -167, 145];
  $asc = bitonic_sort($data, 0, count($data), 1);
  echo rtrim(_str($asc)), PHP_EOL;
  $desc = bitonic_merge($asc, 0, count($asc), 0);
  echo rtrim(_str($desc)), PHP_EOL;
}
main();
