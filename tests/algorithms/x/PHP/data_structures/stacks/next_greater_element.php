<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
$arr = [-10.0, -5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, 3.0, 4.0, -21.0, -10.0, -5.0, -1.0, 0.0];
$expected = [-5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, -1.0, 4.0, -1.0, -10.0, -5.0, -1.0, 0.0, -1.0];
function next_greatest_element_slow($xs) {
  global $arr, $expected;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $next = -1.0;
  $j = $i + 1;
  while ($j < count($xs)) {
  if ($xs[$i] < $xs[$j]) {
  $next = $xs[$j];
  break;
}
  $j = $j + 1;
};
  $res = _append($res, $next);
  $i = $i + 1;
};
  return $res;
}
function next_greatest_element_fast($xs) {
  global $arr, $expected;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  $next = -1.0;
  $j = $i + 1;
  while ($j < count($xs)) {
  $inner = $xs[$j];
  if ($xs[$i] < $inner) {
  $next = $inner;
  break;
}
  $j = $j + 1;
};
  $res = _append($res, $next);
  $i = $i + 1;
};
  return $res;
}
function set_at_float($xs, $idx, $value) {
  global $arr, $expected;
  $i = 0;
  $res = [];
  while ($i < count($xs)) {
  if ($i == $idx) {
  $res = _append($res, $value);
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function next_greatest_element($xs) {
  global $arr, $expected;
  $res = [];
  $k = 0;
  while ($k < count($xs)) {
  $res = _append($res, -1.0);
  $k = $k + 1;
};
  $stack = [];
  $i = 0;
  while ($i < count($xs)) {
  while (count($stack) > 0 && $xs[$i] > $xs[$stack[count($stack) - 1]]) {
  $idx = $stack[count($stack) - 1];
  $stack = array_slice($stack, 0, count($stack) - 1);
  $res = set_at_float($res, $idx, $xs[$i]);
};
  $stack = _append($stack, $i);
  $i = $i + 1;
};
  return $res;
}
echo rtrim(_str(next_greatest_element_slow($arr))), PHP_EOL;
echo rtrim(_str(next_greatest_element_fast($arr))), PHP_EOL;
echo rtrim(_str(next_greatest_element($arr))), PHP_EOL;
