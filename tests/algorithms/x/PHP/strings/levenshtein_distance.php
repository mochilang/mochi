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
function range_list($n) {
  $lst = [];
  $i = 0;
  while ($i < $n) {
  $lst = _append($lst, $i);
  $i = _iadd($i, 1);
};
  return $lst;
}
function min3($a, $b, $c) {
  $m = $a;
  if ($b < $m) {
  $m = $b;
}
  if ($c < $m) {
  $m = $c;
}
  return $m;
}
function levenshtein_distance($first_word, $second_word) {
  if (strlen($first_word) < strlen($second_word)) {
  return levenshtein_distance($second_word, $first_word);
}
  if (strlen($second_word) == 0) {
  return strlen($first_word);
}
  $previous_row = range_list(_iadd(strlen($second_word), 1));
  $i = 0;
  while ($i < strlen($first_word)) {
  $c1 = substr($first_word, $i, $i + 1 - $i);
  $current_row = [];
  $current_row = _append($current_row, _iadd($i, 1));
  $j = 0;
  while ($j < strlen($second_word)) {
  $c2 = substr($second_word, $j, $j + 1 - $j);
  $insertions = _iadd($previous_row[_iadd($j, 1)], 1);
  $deletions = _iadd($current_row[$j], 1);
  $substitutions = _iadd($previous_row[$j], (($c1 == $c2 ? 0 : 1)));
  $min_val = min3($insertions, $deletions, $substitutions);
  $current_row = _append($current_row, $min_val);
  $j = _iadd($j, 1);
};
  $previous_row = $current_row;
  $i = _iadd($i, 1);
};
  return $previous_row[_isub(count($previous_row), 1)];
}
function levenshtein_distance_optimized($first_word, $second_word) {
  if (strlen($first_word) < strlen($second_word)) {
  return levenshtein_distance_optimized($second_word, $first_word);
}
  if (strlen($second_word) == 0) {
  return strlen($first_word);
}
  $previous_row = range_list(_iadd(strlen($second_word), 1));
  $i = 0;
  while ($i < strlen($first_word)) {
  $c1 = substr($first_word, $i, $i + 1 - $i);
  $current_row = [];
  $current_row = _append($current_row, _iadd($i, 1));
  $k = 0;
  while ($k < strlen($second_word)) {
  $current_row = _append($current_row, 0);
  $k = _iadd($k, 1);
};
  $j = 0;
  while ($j < strlen($second_word)) {
  $c2 = substr($second_word, $j, $j + 1 - $j);
  $insertions = _iadd($previous_row[_iadd($j, 1)], 1);
  $deletions = _iadd($current_row[$j], 1);
  $substitutions = _iadd($previous_row[$j], (($c1 == $c2 ? 0 : 1)));
  $min_val = min3($insertions, $deletions, $substitutions);
  $current_row[_iadd($j, 1)] = $min_val;
  $j = _iadd($j, 1);
};
  $previous_row = $current_row;
  $i = _iadd($i, 1);
};
  return $previous_row[_isub(count($previous_row), 1)];
}
function main() {
  $a = 'kitten';
  $b = 'sitting';
  echo rtrim(_str(levenshtein_distance($a, $b))), PHP_EOL;
  echo rtrim(_str(levenshtein_distance_optimized($a, $b))), PHP_EOL;
}
main();
