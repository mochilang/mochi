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
$LETTERS_AND_SPACE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 	
';
$LOWER = 'abcdefghijklmnopqrstuvwxyz';
$UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function to_upper($s) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, _iadd($i, 1) - $i);
  $j = 0;
  $up = $c;
  while ($j < strlen($LOWER)) {
  if ($c == substr($LOWER, $j, _iadd($j, 1) - $j)) {
  $up = substr($UPPER, $j, _iadd($j, 1) - $j);
  break;
}
  $j = _iadd($j, 1);
};
  $res = $res . $up;
  $i = _iadd($i, 1);
};
  return $res;
}
function char_in($chars, $c) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $i = 0;
  while ($i < strlen($chars)) {
  if (substr($chars, $i, _iadd($i, 1) - $i) == $c) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
}
function remove_non_letters($message) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $res = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, _iadd($i, 1) - $i);
  if (char_in($LETTERS_AND_SPACE, $ch)) {
  $res = $res . $ch;
}
  $i = _iadd($i, 1);
};
  return $res;
}
function split_spaces($text) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, _iadd($i, 1) - $i);
  if ($ch == ' ') {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = _iadd($i, 1);
};
  $res = _append($res, $current);
  return $res;
}
function load_dictionary() {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $words = ['HELLO', 'WORLD', 'HOW', 'ARE', 'YOU', 'THE', 'QUICK', 'BROWN', 'FOX', 'JUMPS', 'OVER', 'LAZY', 'DOG'];
  $dict = [];
  foreach ($words as $w) {
  $dict[$w] = true;
};
  return $dict;
}
$ENGLISH_WORDS = load_dictionary();
function get_english_count($message) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $upper = to_upper($message);
  $cleaned = remove_non_letters($upper);
  $possible = split_spaces($cleaned);
  $matches = 0;
  $total = 0;
  foreach ($possible as $w) {
  if ($w != '') {
  $total = _iadd($total, 1);
  if (array_key_exists($w, $ENGLISH_WORDS)) {
  $matches = _iadd($matches, 1);
};
}
};
  if ($total == 0) {
  return 0.0;
}
  return (floatval($matches)) / (floatval($total));
}
function is_english($message, $word_percentage, $letter_percentage) {
  global $LETTERS_AND_SPACE, $LOWER, $UPPER, $ENGLISH_WORDS;
  $words_match = get_english_count($message) * 100.0 >= (floatval($word_percentage));
  $num_letters = strlen(remove_non_letters($message));
  $letters_pct = (strlen($message) == 0 ? 0.0 : (floatval($num_letters)) / (floatval(strlen($message))) * 100.0);
  $letters_match = $letters_pct >= (floatval($letter_percentage));
  return $words_match && $letters_match;
}
echo rtrim(_str(is_english('Hello World', 20, 85))), PHP_EOL;
echo rtrim(_str(is_english('llold HorWd', 20, 85))), PHP_EOL;
