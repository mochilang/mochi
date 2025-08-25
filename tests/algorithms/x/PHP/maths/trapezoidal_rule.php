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
function f($x) {
  global $a, $b, $boundary, $steps, $y;
  return $x * $x;
}
function make_points($a, $b, $h) {
  global $boundary, $steps, $y;
  $xs = [];
  $x = $a + $h;
  while ($x <= ($b - $h)) {
  $xs = _append($xs, $x);
  $x = $x + $h;
};
  return $xs;
}
function trapezoidal_rule($boundary, $steps) {
  $h = ($boundary[1] - $boundary[0]) / $steps;
  $a = $boundary[0];
  $b = $boundary[1];
  $xs = make_points($a, $b, $h);
  $y = ($h / 2.0) * f($a);
  $i = 0;
  while ($i < count($xs)) {
  $y = $y + $h * f($xs[$i]);
  $i = $i + 1;
};
  $y = $y + ($h / 2.0) * f($b);
  return $y;
}
$a = 0.0;
$b = 1.0;
$steps = 10.0;
$boundary = [$a, $b];
$y = trapezoidal_rule($boundary, $steps);
echo rtrim('y = ' . _str($y)), PHP_EOL;
