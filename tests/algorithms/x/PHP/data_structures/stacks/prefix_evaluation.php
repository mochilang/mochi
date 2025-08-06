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
function split($s, $sep) {
  global $test_expression, $test_expression2, $test_expression3;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
}
function tokenize($s) {
  global $test_expression, $test_expression2, $test_expression3;
  $parts = explode(' ', $s);
  $res = [];
  $i = 0;
  while ($i < count($parts)) {
  $p = $parts[$i];
  if ($p != '') {
  $res = _append($res, $p);
}
  $i = $i + 1;
};
  return $res;
}
function is_digit($ch) {
  global $test_expression, $test_expression2, $test_expression3;
  return $ch >= '0' && $ch <= '9';
}
function is_operand($token) {
  global $test_expression, $test_expression2, $test_expression3;
  if ($token == '') {
  return false;
}
  $i = 0;
  while ($i < strlen($token)) {
  $ch = substr($token, $i, $i + 1 - $i);
  if (!is_digit($ch)) {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function to_int($token) {
  global $test_expression, $test_expression2, $test_expression3;
  $res = 0;
  $i = 0;
  while ($i < strlen($token)) {
  $res = $res * 10 + (ord(substr($token, $i, $i + 1 - $i)));
  $i = $i + 1;
};
  return $res;
}
function apply_op($op, $a, $b) {
  global $test_expression, $test_expression2, $test_expression3;
  if ($op == '+') {
  return $a + $b;
}
  if ($op == '-') {
  return $a - $b;
}
  if ($op == '*') {
  return $a * $b;
}
  if ($op == '/') {
  return $a / $b;
}
  return 0.0;
}
function evaluate($expression) {
  global $test_expression, $test_expression2, $test_expression3;
  $tokens = tokenize($expression);
  $stack = [];
  $i = count($tokens) - 1;
  while ($i >= 0) {
  $token = $tokens[$i];
  if ($token != '') {
  if (is_operand($token)) {
  $stack = _append($stack, (floatval(to_int($token))));
} else {
  $o1 = $stack[count($stack) - 1];
  $o2 = $stack[count($stack) - 2];
  $stack = array_slice($stack, 0, count($stack) - 2 - 0);
  $res = apply_op($token, $o1, $o2);
  $stack = _append($stack, $res);
};
}
  $i = $i - 1;
};
  return $stack[0];
}
function eval_rec($tokens, $pos) {
  global $test_expression, $test_expression2, $test_expression3;
  $token = $tokens[$pos];
  $next = $pos + 1;
  if (is_operand($token)) {
  return [(floatval(to_int($token))), (floatval($next))];
}
  $left = eval_rec($tokens, $next);
  $a = $left[0];
  $p1 = intval($left[1]);
  $right = eval_rec($tokens, $p1);
  $b = $right[0];
  $p2 = $right[1];
  return [apply_op($token, $a, $b), $p2];
}
function evaluate_recursive($expression) {
  global $test_expression, $test_expression2, $test_expression3;
  $tokens = tokenize($expression);
  $res = eval_rec($tokens, 0);
  return $res[0];
}
$test_expression = '+ 9 * 2 6';
echo rtrim(_str(evaluate($test_expression))), PHP_EOL;
$test_expression2 = '/ * 10 2 + 4 1 ';
echo rtrim(_str(evaluate($test_expression2))), PHP_EOL;
$test_expression3 = '+ * 2 3 / 8 4';
echo rtrim(_str(evaluate_recursive($test_expression3))), PHP_EOL;
