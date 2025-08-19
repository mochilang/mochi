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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$PI = 3.141592653589793;
$G = 9.80665;
function mochi_sqrt($x) {
  global $G, $PI;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function period_of_pendulum($length) {
  global $G, $PI;
  if ($length < 0.0) {
  _panic('The length should be non-negative');
}
  return 2.0 * $PI * mochi_sqrt($length / $G);
}
echo rtrim(_str(period_of_pendulum(1.23))), PHP_EOL;
echo rtrim(_str(period_of_pendulum(2.37))), PHP_EOL;
echo rtrim(_str(period_of_pendulum(5.63))), PHP_EOL;
echo rtrim(_str(period_of_pendulum(0.0))), PHP_EOL;
