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
function parse_names($line) {
  $names = [];
  $current = '';
  $i = 0;
  while ($i < strlen($line)) {
  $ch = substr($line, $i, _iadd($i, 1) - $i);
  if ($ch == ',') {
  $names = _append($names, $current);
  $current = '';
} else {
  if ($ch != '"') {
  $current = $current . $ch;
};
}
  $i = _iadd($i, 1);
};
  $names = _append($names, $current);
  return $names;
}
function insertion_sort($arr) {
  $a = $arr;
  $i = 1;
  while ($i < count($a)) {
  $key = $a[$i];
  $j = _isub($i, 1);
  while ($j >= 0 && $a[$j] > $key) {
  $a[_iadd($j, 1)] = $a[$j];
  $j = _isub($j, 1);
};
  $a[_iadd($j, 1)] = $key;
  $i = _iadd($i, 1);
};
  return $a;
}
function letter_value($ch) {
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $idx = 0;
  while ($idx < strlen($alphabet)) {
  if (substr($alphabet, $idx, _iadd($idx, 1) - $idx) == $ch) {
  return _iadd($idx, 1);
}
  $idx = _iadd($idx, 1);
};
  return 0;
}
function name_score($name) {
  $score = 0;
  $i = 0;
  while ($i < strlen($name)) {
  $score = _iadd($score, letter_value(substr($name, $i, _iadd($i, 1) - $i)));
  $i = _iadd($i, 1);
};
  return $score;
}
function main() {
  $line = trim(fgets(STDIN));
  $names = insertion_sort(parse_names($line));
  $total = 0;
  $i = 0;
  while ($i < count($names)) {
  $total = _iadd($total, _imul((_iadd($i, 1)), name_score($names[$i])));
  $i = _iadd($i, 1);
};
  echo rtrim(_str($total)), PHP_EOL;
}
main();
