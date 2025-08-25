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
function expApprox($x) {
  $neg = false;
  $y = $x;
  if ($x < 0.0) {
  $neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  if ($neg) {
  return 1.0 / $sum;
}
  return $sum;
}
function tangent_hyperbolic($vector) {
  $result = [];
  $i = 0;
  while ($i < count($vector)) {
  $x = $vector[$i];
  $t = (2.0 / (1.0 + expApprox(-2.0 * $x))) - 1.0;
  $result = _append($result, $t);
  $i = $i + 1;
};
  return $result;
}
function main() {
  $v1 = [1.0, 5.0, 6.0, -0.67];
  $v2 = [8.0, 10.0, 2.0, -0.98, 13.0];
  echo rtrim(_str(tangent_hyperbolic($v1))), PHP_EOL;
  echo rtrim(_str(tangent_hyperbolic($v2))), PHP_EOL;
}
main();
