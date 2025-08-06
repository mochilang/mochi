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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function is_digit($ch) {
  global $equation;
  return $ch == '0' || $ch == '1' || $ch == '2' || $ch == '3' || $ch == '4' || $ch == '5' || $ch == '6' || $ch == '7' || $ch == '8' || $ch == '9';
}
function slice_without_last_int($xs) {
  global $equation;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function slice_without_last_string($xs) {
  global $equation;
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function dijkstras_two_stack_algorithm($equation) {
  $operand_stack = [];
  $operator_stack = [];
  $idx = 0;
  while ($idx < strlen($equation)) {
  $ch = substr($equation, $idx, $idx + 1 - $idx);
  if (is_digit($ch)) {
  $operand_stack = _append($operand_stack, intval($ch));
} else {
  if ($ch == '+' || $ch == '-' || $ch == '*' || $ch == '/') {
  $operator_stack = _append($operator_stack, $ch);
} else {
  if ($ch == ')') {
  $opr = $operator_stack[count($operator_stack) - 1];
  $operator_stack = slice_without_last_string($operator_stack);
  $num1 = $operand_stack[count($operand_stack) - 1];
  $operand_stack = slice_without_last_int($operand_stack);
  $num2 = $operand_stack[count($operand_stack) - 1];
  $operand_stack = slice_without_last_int($operand_stack);
  $total = ($opr == '+' ? $num2 + $num1 : ($opr == '-' ? $num2 - $num1 : ($opr == '*' ? $num2 * $num1 : _intdiv($num2, $num1))));
  $operand_stack = _append($operand_stack, $total);
};
};
}
  $idx = $idx + 1;
};
  return $operand_stack[count($operand_stack) - 1];
}
$equation = '(5 + ((4 * 2) * (2 + 3)))';
echo rtrim($equation . ' = ' . _str(dijkstras_two_stack_algorithm($equation))), PHP_EOL;
