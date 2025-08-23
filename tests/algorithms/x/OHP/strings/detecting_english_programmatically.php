<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
$__start_mem = memory_get_usage();
$__start = _now();
  $LETTERS_AND_SPACE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz 	
';
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  function to_upper($s) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $up = $c;
  while ($j < strlen($LOWER)) {
  if ($c == substr($LOWER, $j, $j + 1 - $j)) {
  $up = substr($UPPER, $j, $j + 1 - $j);
  break;
}
  $j = $j + 1;
};
  $res = $res . $up;
  $i = $i + 1;
};
  return $res;
};
  function char_in($chars, $c) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $i = 0;
  while ($i < strlen($chars)) {
  if (substr($chars, $i, $i + 1 - $i) == $c) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function remove_non_letters($message) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $res = '';
  $i = 0;
  while ($i < strlen($message)) {
  $ch = substr($message, $i, $i + 1 - $i);
  if (char_in($LETTERS_AND_SPACE, $ch)) {
  $res = $res . $ch;
}
  $i = $i + 1;
};
  return $res;
};
  function split_spaces($text) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  if ($ch == ' ') {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function load_dictionary() {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $words = ['HELLO', 'WORLD', 'HOW', 'ARE', 'YOU', 'THE', 'QUICK', 'BROWN', 'FOX', 'JUMPS', 'OVER', 'LAZY', 'DOG'];
  $dict = [];
  foreach ($words as $w) {
  $dict[$w] = true;
};
  return $dict;
};
  $ENGLISH_WORDS = load_dictionary();
  function get_english_count($message) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $upper = to_upper($message);
  $cleaned = remove_non_letters($upper);
  $possible = split_spaces($cleaned);
  $matches = 0;
  $total = 0;
  foreach ($possible as $w) {
  if ($w != '') {
  $total = $total + 1;
  if (array_key_exists($w, $ENGLISH_WORDS)) {
  $matches = $matches + 1;
};
}
};
  if ($total == 0) {
  return 0.0;
}
  return (floatval($matches)) / (floatval($total));
};
  function is_english($message, $word_percentage, $letter_percentage) {
  global $ENGLISH_WORDS, $LETTERS_AND_SPACE, $LOWER, $UPPER;
  $words_match = get_english_count($message) * 100.0 >= (floatval($word_percentage));
  $num_letters = strlen(remove_non_letters($message));
  $letters_pct = (strlen($message) == 0 ? 0.0 : (floatval($num_letters)) / (floatval(strlen($message))) * 100.0);
  $letters_match = $letters_pct >= (floatval($letter_percentage));
  return $words_match && $letters_match;
};
  echo rtrim(_str(is_english('Hello World', 20, 85))), PHP_EOL;
  echo rtrim(_str(is_english('llold HorWd', 20, 85))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
