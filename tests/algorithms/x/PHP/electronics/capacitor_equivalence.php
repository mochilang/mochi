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
function capacitor_parallel($capacitors) {
  $sum_c = 0.0;
  $i = 0;
  while ($i < count($capacitors)) {
  $c = $capacitors[$i];
  if ($c < 0.0) {
  $panic('Capacitor at index ' . _str($i) . ' has a negative value!');
  return 0.0;
}
  $sum_c = $sum_c + $c;
  $i = $i + 1;
};
  return $sum_c;
}
function capacitor_series($capacitors) {
  $first_sum = 0.0;
  $i = 0;
  while ($i < count($capacitors)) {
  $c = $capacitors[$i];
  if ($c <= 0.0) {
  $panic('Capacitor at index ' . _str($i) . ' has a negative or zero value!');
  return 0.0;
}
  $first_sum = $first_sum + 1.0 / $c;
  $i = $i + 1;
};
  return 1.0 / $first_sum;
}
function main() {
  $parallel = capacitor_parallel([5.71389, 12.0, 3.0]);
  $series = capacitor_series([5.71389, 12.0, 3.0]);
  echo rtrim(_str($parallel)), PHP_EOL;
  echo rtrim(_str($series)), PHP_EOL;
}
main();
