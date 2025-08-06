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
$ELECTRON_CHARGE = 0.00000000000000000016021;
function electric_conductivity($conductivity, $electron_conc, $mobility) {
  global $ELECTRON_CHARGE, $r1, $r2, $r3;
  $zero_count = 0;
  if ($conductivity == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($electron_conc == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($mobility == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  $panic('You cannot supply more or less than 2 values');
}
  if ($conductivity < 0.0) {
  $panic('Conductivity cannot be negative');
}
  if ($electron_conc < 0.0) {
  $panic('Electron concentration cannot be negative');
}
  if ($mobility < 0.0) {
  $panic('mobility cannot be negative');
}
  if ($conductivity == 0.0) {
  return ['kind' => 'conductivity', 'value' => $mobility * $electron_conc * $ELECTRON_CHARGE];
}
  if ($electron_conc == 0.0) {
  return ['kind' => 'electron_conc', 'value' => $conductivity / ($mobility * $ELECTRON_CHARGE)];
}
  return ['kind' => 'mobility', 'value' => $conductivity / ($electron_conc * $ELECTRON_CHARGE)];
}
$r1 = electric_conductivity(25.0, 100.0, 0.0);
$r2 = electric_conductivity(0.0, 1600.0, 200.0);
$r3 = electric_conductivity(1000.0, 0.0, 1200.0);
echo rtrim($r1['kind'] . ' ' . _str($r1['value'])), PHP_EOL;
echo rtrim($r2['kind'] . ' ' . _str($r2['value'])), PHP_EOL;
echo rtrim($r3['kind'] . ' ' . _str($r3['value'])), PHP_EOL;
