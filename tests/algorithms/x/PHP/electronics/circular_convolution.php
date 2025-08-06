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
function mochi_floor($x) {
  global $example1, $example2, $example3, $example4;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
}
function pow10($n) {
  global $example1, $example2, $example3, $example4;
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
}
function roundn($x, $n) {
  global $example1, $example2, $example3, $example4;
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
}
function pad($signal, $target) {
  global $example1, $example2, $example3, $example4;
  $s = $signal;
  while (count($s) < $target) {
  $s = _append($s, 0.0);
};
  return $s;
}
function circular_convolution($a, $b) {
  global $example1, $example2, $example3, $example4;
  $n1 = count($a);
  $n2 = count($b);
  $n = ($n1 > $n2 ? $n1 : $n2);
  $x = pad($a, $n);
  $y = pad($b, $n);
  $res = [];
  $i = 0;
  while ($i < $n) {
  $sum = 0.0;
  $k = 0;
  while ($k < $n) {
  $j = ($i - $k) % $n;
  $idx = ($j < 0 ? $j + $n : $j);
  $sum = $sum + $x[$k] * $y[$idx];
  $k = $k + 1;
};
  $res = _append($res, roundn($sum, 2));
  $i = $i + 1;
};
  return $res;
}
$example1 = circular_convolution([2.0, 1.0, 2.0, -1.0], [1.0, 2.0, 3.0, 4.0]);
echo rtrim(_str($example1)), PHP_EOL;
$example2 = circular_convolution([0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6], [0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5]);
echo rtrim(_str($example2)), PHP_EOL;
$example3 = circular_convolution([-1.0, 1.0, 2.0, -2.0], [0.5, 1.0, -1.0, 2.0, 0.75]);
echo rtrim(_str($example3)), PHP_EOL;
$example4 = circular_convolution([1.0, -1.0, 2.0, 3.0, -1.0], [1.0, 2.0, 3.0]);
echo rtrim(_str($example4)), PHP_EOL;
