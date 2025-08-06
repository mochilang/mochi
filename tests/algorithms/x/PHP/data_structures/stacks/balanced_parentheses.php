<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function pop_last($xs) {
  global $tests, $idx;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function balanced_parentheses($s) {
  global $tests, $idx;
  $stack = [];
  $pairs = ['(' => ')', '[' => ']', '{' => '}'];
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if (array_key_exists($ch, $pairs)) {
  $stack = _append($stack, $ch);
} else {
  if ($ch == ')' || $ch == ']' || $ch == '}') {
  if (count($stack) == 0) {
  return false;
};
  $top = $stack[count($stack) - 1];
  if ($pairs[$top] != $ch) {
  return false;
};
  $stack = pop_last($stack);
};
}
  $i = $i + 1;
};
  return count($stack) == 0;
}
$tests = ['([]{})', '[()]{}{[()()]()}', '[(])', '1+2*3-4', ''];
$idx = 0;
while ($idx < count($tests)) {
  echo rtrim(json_encode(balanced_parentheses($tests[$idx]), 1344)), PHP_EOL;
  $idx = $idx + 1;
}
