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
function slice_without_last($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function parse_float($token) {
  $sign = 1.0;
  $idx = 0;
  if (strlen($token) > 0) {
  $first = substr($token, 0, 1 - 0);
  if ($first == '-') {
  $sign = -1.0;
  $idx = 1;
} else {
  if ($first == '+') {
  $idx = 1;
};
};
}
  $int_part = 0;
  while ($idx < strlen($token) && substr($token, $idx, $idx + 1 - $idx) != '.') {
  $int_part = $int_part * 10 + intval(substr($token, $idx, $idx + 1 - $idx));
  $idx = $idx + 1;
};
  $result = 1.0 * $int_part;
  if ($idx < strlen($token) && substr($token, $idx, $idx + 1 - $idx) == '.') {
  $idx = $idx + 1;
  $place = 0.1;
  while ($idx < strlen($token)) {
  $digit = intval(substr($token, $idx, $idx + 1 - $idx));
  $result = $result + $place * (1.0 * $digit);
  $place = $place / 10.0;
  $idx = $idx + 1;
};
}
  return $sign * $result;
}
function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  $e = intval($exp);
  while ($i < $e) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function apply_op($a, $b, $op) {
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
  if ($op == '^') {
  return pow_float($a, $b);
}
  return 0.0;
}
function evaluate($tokens) {
  if (count($tokens) == 0) {
  return 0.0;
}
  $stack = [];
  foreach ($tokens as $token) {
  if ($token == '+' || $token == '-' || $token == '*' || $token == '/' || $token == '^') {
  if (($token == '+' || $token == '-') && count($stack) < 2) {
  $b = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  if ($token == '-') {
  $stack = _append($stack, 0.0 - $b);
} else {
  $stack = _append($stack, $b);
};
} else {
  $b = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  $a = $stack[count($stack) - 1];
  $stack = slice_without_last($stack);
  $result = apply_op($a, $b, $token);
  $stack = _append($stack, $result);
};
} else {
  $stack = _append($stack, parse_float($token));
}
};
  if (count($stack) != 1) {
  $panic('Invalid postfix expression');
}
  return $stack[0];
}
echo rtrim(_str(evaluate(['2', '1', '+', '3', '*']))), PHP_EOL;
echo rtrim(_str(evaluate(['4', '13', '5', '/', '+']))), PHP_EOL;
echo rtrim(_str(evaluate(['5', '6', '9', '*', '+']))), PHP_EOL;
echo rtrim(_str(evaluate(['2', '-', '3', '+']))), PHP_EOL;
echo rtrim(_str(evaluate([]))), PHP_EOL;
