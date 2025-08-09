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
function ord_letter($ch) {
  global $rows, $names, $total;
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $i = 0;
  while ($i < strlen($alphabet)) {
  if (substr($alphabet, $i, _iadd($i, 1) - $i) == $ch) {
  return _iadd($i, 1);
}
  $i = _iadd($i, 1);
};
  return 0;
}
function name_value($name) {
  global $rows, $names;
  $total = 0;
  $i = 0;
  while ($i < strlen($name)) {
  $total = _iadd($total, ord_letter(substr($name, $i, _iadd($i, 1) - $i)));
  $i = _iadd($i, 1);
};
  return $total;
}
function bubble_sort(&$arr) {
  global $rows, $names, $total;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j < _isub(_isub($n, $i), 1)) {
  if ($arr[$j] > $arr[_iadd($j, 1)]) {
  $temp = $arr[$j];
  $arr[$j] = $arr[_iadd($j, 1)];
  $arr[_iadd($j, 1)] = $temp;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $arr;
}
$rows = (function() { $rows = []; foreach (file("tests/github/TheAlgorithms/Mochi/project_euler/problem_022/p022_names.jsonl", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES) as $line) { $line = trim($line); if ($line === '') continue; $rows[] = json_decode($line, true); } return $rows; })();
$names = [];
foreach ($rows as $r) {
  $names = _append($names, $r['name']);
}
$names = bubble_sort($names);
$total = 0;
$i = 0;
while ($i < count($names)) {
  $total = _iadd($total, _imul((_iadd($i, 1)), name_value($names[$i])));
  $i = _iadd($i, 1);
}
echo rtrim(_str($total)), PHP_EOL;
