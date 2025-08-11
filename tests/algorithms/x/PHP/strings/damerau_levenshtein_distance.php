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
function damerau_levenshtein_distance($first_string, $second_string) {
  $len1 = strlen($first_string);
  $len2 = strlen($second_string);
  $dp_matrix = [];
  for ($_ = 0; $_ < (_iadd($len1, 1)); $_++) {
  $row = [];
  for ($_2 = 0; $_2 < (_iadd($len2, 1)); $_2++) {
  $row = _append($row, 0);
};
  $dp_matrix = _append($dp_matrix, $row);
};
  for ($i = 0; $i < (_iadd($len1, 1)); $i++) {
  $row = $dp_matrix[$i];
  $row[0] = $i;
  $dp_matrix[$i] = $row;
};
  $first_row = $dp_matrix[0];
  for ($j = 0; $j < (_iadd($len2, 1)); $j++) {
  $first_row[$j] = $j;
};
  $dp_matrix[0] = $first_row;
  for ($i = 1; $i < (_iadd($len1, 1)); $i++) {
  $row = $dp_matrix[$i];
  $first_char = substr($first_string, _isub($i, 1), $i - _isub($i, 1));
  for ($j = 1; $j < (_iadd($len2, 1)); $j++) {
  $second_char = substr($second_string, _isub($j, 1), $j - _isub($j, 1));
  $cost = ($first_char == $second_char ? 0 : 1);
  $value = _iadd($dp_matrix[_isub($i, 1)][$j], 1);
  $insertion = _iadd($row[_isub($j, 1)], 1);
  if ($insertion < $value) {
  $value = $insertion;
}
  $substitution = _iadd($dp_matrix[_isub($i, 1)][_isub($j, 1)], $cost);
  if ($substitution < $value) {
  $value = $substitution;
}
  $row[$j] = $value;
  if ($i > 1 && $j > 1 && substr($first_string, _isub($i, 1), $i - _isub($i, 1)) == substr($second_string, _isub($j, 2), _isub($j, 1) - _isub($j, 2)) && substr($first_string, _isub($i, 2), _isub($i, 1) - _isub($i, 2)) == substr($second_string, _isub($j, 1), $j - _isub($j, 1))) {
  $transposition = _iadd($dp_matrix[_isub($i, 2)][_isub($j, 2)], $cost);
  if ($transposition < $row[$j]) {
  $row[$j] = $transposition;
};
}
};
  $dp_matrix[$i] = $row;
};
  return $dp_matrix[$len1][$len2];
}
echo rtrim(_str(damerau_levenshtein_distance('cat', 'cut'))), PHP_EOL;
echo rtrim(_str(damerau_levenshtein_distance('kitten', 'sitting'))), PHP_EOL;
echo rtrim(_str(damerau_levenshtein_distance('hello', 'world'))), PHP_EOL;
echo rtrim(_str(damerau_levenshtein_distance('book', 'back'))), PHP_EOL;
echo rtrim(_str(damerau_levenshtein_distance('container', 'containment'))), PHP_EOL;
