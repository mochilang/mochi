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
$COULOMBS_CONSTANT = 8988000000.0;
function mochi_abs($x) {
  global $COULOMBS_CONSTANT;
  if ($x < 0.0) {
  return -$x;
}
  return $x;
}
function sqrtApprox($x) {
  global $COULOMBS_CONSTANT;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function coulombs_law($force, $charge1, $charge2, $distance) {
  global $COULOMBS_CONSTANT;
  $charge_product = mochi_abs($charge1 * $charge2);
  $zero_count = 0;
  if ($force == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($charge1 == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($charge2 == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($distance == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  $panic('One and only one argument must be 0');
}
  if ($distance < 0.0) {
  $panic('Distance cannot be negative');
}
  if ($force == 0.0) {
  $f = $COULOMBS_CONSTANT * $charge_product / ($distance * $distance);
  return ['force' => $f];
}
  if ($charge1 == 0.0) {
  $c1 = mochi_abs($force) * ($distance * $distance) / ($COULOMBS_CONSTANT * $charge2);
  return ['charge1' => $c1];
}
  if ($charge2 == 0.0) {
  $c2 = mochi_abs($force) * ($distance * $distance) / ($COULOMBS_CONSTANT * $charge1);
  return ['charge2' => $c2];
}
  $d = sqrtApprox($COULOMBS_CONSTANT * $charge_product / mochi_abs($force));
  return ['distance' => $d];
}
function print_map($m) {
  global $COULOMBS_CONSTANT;
  foreach (array_keys($m) as $k) {
  echo rtrim('{"' . $k . '": ' . _str($m[$k]) . '}'), PHP_EOL;
};
}
print_map(coulombs_law(0.0, 3.0, 5.0, 2000.0));
print_map(coulombs_law(10.0, 3.0, 5.0, 0.0));
print_map(coulombs_law(10.0, 0.0, 5.0, 2000.0));
