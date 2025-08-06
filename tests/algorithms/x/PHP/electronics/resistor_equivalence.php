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
function resistor_parallel($resistors) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($resistors)) {
  $r = $resistors[$i];
  if ($r <= 0.0) {
  $panic('Resistor at index ' . _str($i) . ' has a negative or zero value!');
}
  $sum = $sum + 1.0 / $r;
  $i = $i + 1;
};
  return 1.0 / $sum;
}
function resistor_series($resistors) {
  $sum = 0.0;
  $i = 0;
  while ($i < count($resistors)) {
  $r = $resistors[$i];
  if ($r < 0.0) {
  $panic('Resistor at index ' . _str($i) . ' has a negative value!');
}
  $sum = $sum + $r;
  $i = $i + 1;
};
  return $sum;
}
function main() {
  $resistors = [3.21389, 2.0, 3.0];
  echo rtrim('Parallel: ' . _str(resistor_parallel($resistors))), PHP_EOL;
  echo rtrim('Series: ' . _str(resistor_series($resistors))), PHP_EOL;
}
main();
