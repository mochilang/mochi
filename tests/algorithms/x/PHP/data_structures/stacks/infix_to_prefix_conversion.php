<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$PRIORITY = ['^' => 3, '*' => 2, '/' => 2, '%' => 2, '+' => 1, '-' => 1];
$LETTERS = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
$DIGITS = '0123456789';
function is_alpha($ch) {
  global $PRIORITY, $LETTERS, $DIGITS;
  $i = 0;
  while ($i < strlen($LETTERS)) {
  if (substr($LETTERS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function is_digit($ch) {
  global $PRIORITY, $LETTERS, $DIGITS;
  $i = 0;
  while ($i < strlen($DIGITS)) {
  if (substr($DIGITS, $i, $i + 1 - $i) == $ch) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function reverse_string($s) {
  global $PRIORITY, $LETTERS, $DIGITS;
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
}
function infix_to_postfix($infix) {
  global $PRIORITY, $LETTERS, $DIGITS;
  $stack = [];
  $post = [];
  $i = 0;
  while ($i < strlen($infix)) {
  $x = substr($infix, $i, $i + 1 - $i);
  if (is_alpha($x) || is_digit($x)) {
  $post = _append($post, $x);
} else {
  if ($x == '(') {
  $stack = _append($stack, $x);
} else {
  if ($x == ')') {
  if (count($stack) == 0) {
  $panic('list index out of range');
};
  while ($stack[count($stack) - 1] != '(') {
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
} else {
  if (count($stack) == 0) {
  $stack = _append($stack, $x);
} else {
  while (count($stack) > 0 && $stack[count($stack) - 1] != '(' && $PRIORITY[$x] <= $PRIORITY[$stack[count($stack) - 1]]) {
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
  $stack = _append($stack, $x);
};
};
};
}
  $i = $i + 1;
};
  while (count($stack) > 0) {
  if ($stack[count($stack) - 1] == '(') {
  $panic('invalid expression');
}
  $post = _append($post, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
  $res = '';
  $j = 0;
  while ($j < count($post)) {
  $res = $res . $post[$j];
  $j = $j + 1;
};
  return $res;
}
function infix_to_prefix($infix) {
  global $PRIORITY, $LETTERS, $DIGITS;
  $reversed = '';
  $i = strlen($infix) - 1;
  while ($i >= 0) {
  $ch = substr($infix, $i, $i + 1 - $i);
  if ($ch == '(') {
  $reversed = $reversed . ')';
} else {
  if ($ch == ')') {
  $reversed = $reversed . '(';
} else {
  $reversed = $reversed . $ch;
};
}
  $i = $i - 1;
};
  $postfix = infix_to_postfix($reversed);
  $prefix = reverse_string($postfix);
  return $prefix;
}
