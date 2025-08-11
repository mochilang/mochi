<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function split_words($s) {
  $words = [];
  $current = '';
  foreach (str_split($s) as $ch) {
  if ($ch == ' ') {
  if ($current != '') {
  $words = _append($words, $current);
  $current = '';
};
} else {
  $current = $current . $ch;
}
};
  if ($current != '') {
  $words = _append($words, $current);
}
  return $words;
}
function is_alnum($c) {
  return (strpos('0123456789', $c) !== false) || (strpos('abcdefghijklmnopqrstuvwxyz', $c) !== false) || (strpos('ABCDEFGHIJKLMNOPQRSTUVWXYZ', $c) !== false) || $c == ' ';
}
function split_input($text) {
  $result = [];
  $current = '';
  foreach (str_split($text) as $ch) {
  if (is_alnum($ch)) {
  $current = $current . $ch;
} else {
  if ($current != '') {
  $result = _append($result, split_words($current));
  $current = '';
};
}
};
  if ($current != '') {
  $result = _append($result, split_words($current));
}
  return $result;
}
function capitalize($word) {
  if (strlen($word) == 0) {
  return '';
}
  if (strlen($word) == 1) {
  return strtoupper($word);
}
  return strtoupper(substr($word, 0, 1)) . strtolower(substr($word, 1));
}
function to_simple_case($text) {
  $parts = split_input($text);
  $res = '';
  foreach ($parts as $sub) {
  foreach ($sub as $w) {
  $res = $res . capitalize($w);
};
};
  return $res;
}
function to_complex_case($text, $upper_flag, $sep) {
  $parts = split_input($text);
  $res = '';
  foreach ($parts as $sub) {
  $first = true;
  foreach ($sub as $w) {
  $word = ($upper_flag ? strtoupper($w) : strtolower($w));
  if ($first) {
  $res = $res . $word;
  $first = false;
} else {
  $res = $res . $sep . $word;
}
};
};
  return $res;
}
function to_pascal_case($text) {
  return to_simple_case($text);
}
function to_camel_case($text) {
  $s = to_simple_case($text);
  if (strlen($s) == 0) {
  return '';
}
  return strtolower(substr($s, 0, 1)) . substr($s, 1);
}
function to_snake_case($text, $upper_flag) {
  return to_complex_case($text, $upper_flag, '_');
}
function to_kebab_case($text, $upper_flag) {
  return to_complex_case($text, $upper_flag, '-');
}
echo rtrim(to_pascal_case('one two 31235three4four')), PHP_EOL;
echo rtrim(to_camel_case('one two 31235three4four')), PHP_EOL;
echo rtrim(to_snake_case('one two 31235three4four', true)), PHP_EOL;
echo rtrim(to_snake_case('one two 31235three4four', false)), PHP_EOL;
echo rtrim(to_kebab_case('one two 31235three4four', true)), PHP_EOL;
echo rtrim(to_kebab_case('one two 31235three4four', false)), PHP_EOL;
