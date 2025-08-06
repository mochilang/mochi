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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _fib($n) {
  global $i;
  if ($n == 0) {
  return ['fn' => 0, 'fn1' => 1];
}
  $half = _fib(_intdiv($n, 2));
  $a = $half['fn'];
  $b = $half['fn1'];
  $c = $a * ($b * 2 - $a);
  $d = $a * $a + $b * $b;
  if ($n % 2 == 0) {
  return ['fn' => $c, 'fn1' => $d];
}
  return ['fn' => $d, 'fn1' => $c + $d];
}
function fibonacci($n) {
  global $i;
  if ($n < 0) {
  $panic('Negative arguments are not supported');
}
  $res = _fib($n);
  return $res['fn'];
}
$i = 0;
while ($i < 13) {
  echo rtrim(_str(fibonacci($i))), PHP_EOL;
  $i = $i + 1;
}
