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
function find_index($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return $i;
}
  $i = _iadd($i, 1);
};
  return -1;
}
function get_word_pattern($word) {
  $w = strtoupper($word);
  $letters = [];
  $numbers = [];
  $next_num = 0;
  $res = '';
  $i = 0;
  while ($i < strlen($w)) {
  $ch = substr($w, $i, $i + 1 - $i);
  $idx = find_index($letters, $ch);
  $num_str = '';
  if ($idx >= 0) {
  $num_str = $numbers[$idx];
} else {
  $num_str = _str($next_num);
  $letters = _append($letters, $ch);
  $numbers = _append($numbers, $num_str);
  $next_num = _iadd($next_num, 1);
}
  if ($i > 0) {
  $res = $res . '.';
}
  $res = $res . $num_str;
  $i = _iadd($i, 1);
};
  return $res;
}
function main() {
  echo rtrim(get_word_pattern('')), PHP_EOL;
  echo rtrim(get_word_pattern(' ')), PHP_EOL;
  echo rtrim(get_word_pattern('pattern')), PHP_EOL;
  echo rtrim(get_word_pattern('word pattern')), PHP_EOL;
  echo rtrim(get_word_pattern('get word pattern')), PHP_EOL;
}
main();
