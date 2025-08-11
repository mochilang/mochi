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
function split_ws($s) {
  $res = [];
  $word = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, _iadd($i, 1) - $i);
  if ($ch == ' ') {
  if ($word != '') {
  $res = _append($res, $word);
  $word = '';
};
} else {
  $word = $word . $ch;
}
  $i = _iadd($i, 1);
};
  if ($word != '') {
  $res = _append($res, $word);
}
  return $res;
}
function contains($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function unique($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $w = $xs[$i];
  if (!contains($res, $w)) {
  $res = _append($res, $w);
}
  $i = _iadd($i, 1);
};
  return $res;
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
function join_with_space($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $s = $s . ' ';
}
  $s = $s . $xs[$i];
  $i = _iadd($i, 1);
};
  return $s;
}
function remove_duplicates($sentence) {
  $words = split_ws($sentence);
  $uniq = unique($words);
  $sorted_words = insertion_sort($uniq);
  return join_with_space($sorted_words);
}
echo rtrim(remove_duplicates('Python is great and Java is also great')), PHP_EOL;
echo rtrim(remove_duplicates('Python   is      great and Java is also great')), PHP_EOL;
