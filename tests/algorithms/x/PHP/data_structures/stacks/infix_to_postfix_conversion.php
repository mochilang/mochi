<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$PRECEDENCES = ['+' => 1, '-' => 1, '*' => 2, '/' => 2, '^' => 3];
$ASSOCIATIVITIES = ['+' => 'LR', '-' => 'LR', '*' => 'LR', '/' => 'LR', '^' => 'RL'];
function precedence($ch) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  if (array_key_exists($ch, $PRECEDENCES)) {
  return $PRECEDENCES[$ch];
}
  return -1;
}
function associativity($ch) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  if (array_key_exists($ch, $ASSOCIATIVITIES)) {
  return $ASSOCIATIVITIES[$ch];
}
  return '';
}
function balanced_parentheses($expr) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  $count = 0;
  $i = 0;
  while ($i < strlen($expr)) {
  $ch = substr($expr, $i, $i + 1 - $i);
  if ($ch == '(') {
  $count = $count + 1;
}
  if ($ch == ')') {
  $count = $count - 1;
  if ($count < 0) {
  return false;
};
}
  $i = $i + 1;
};
  return $count == 0;
}
function is_letter($ch) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  return ('a' <= $ch && $ch <= 'z') || ('A' <= $ch && $ch <= 'Z');
}
function is_digit($ch) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  return '0' <= $ch && $ch <= '9';
}
function is_alnum($ch) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  return is_letter($ch) || is_digit($ch);
}
function infix_to_postfix($expression) {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  if (balanced_parentheses($expression) == false) {
  $panic('Mismatched parentheses');
}
  $stack = [];
  $postfix = [];
  $i = 0;
  while ($i < strlen($expression)) {
  $ch = substr($expression, $i, $i + 1 - $i);
  if (is_alnum($ch)) {
  $postfix = _append($postfix, $ch);
} else {
  if ($ch == '(') {
  $stack = _append($stack, $ch);
} else {
  if ($ch == ')') {
  while (count($stack) > 0 && $stack[count($stack) - 1] != '(') {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
  if (count($stack) > 0) {
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
} else {
  if ($ch == ' ') {
} else {
  while (true) {
  if (count($stack) == 0) {
  $stack = _append($stack, $ch);
  break;
}
  $cp = precedence($ch);
  $tp = precedence($stack[count($stack) - 1]);
  if ($cp > $tp) {
  $stack = _append($stack, $ch);
  break;
}
  if ($cp < $tp) {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
  continue;
}
  if (associativity($ch) == 'RL') {
  $stack = _append($stack, $ch);
  break;
}
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
};
};
};
}
  $i = $i + 1;
};
  while (count($stack) > 0) {
  $postfix = _append($postfix, $stack[count($stack) - 1]);
  $stack = array_slice($stack, 0, count($stack) - 1 - 0);
};
  $res = '';
  $j = 0;
  while ($j < count($postfix)) {
  if ($j > 0) {
  $res = $res . ' ';
}
  $res = $res . $postfix[$j];
  $j = $j + 1;
};
  return $res;
}
function main() {
  global $PRECEDENCES, $ASSOCIATIVITIES;
  $expression = 'a+b*(c^d-e)^(f+g*h)-i';
  echo rtrim($expression), PHP_EOL;
  echo rtrim(infix_to_postfix($expression)), PHP_EOL;
}
main();
