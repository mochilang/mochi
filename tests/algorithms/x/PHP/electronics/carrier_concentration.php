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
function sqrtApprox($x) {
  global $r1, $r2, $r3;
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function carrier_concentration($electron_conc, $hole_conc, $intrinsic_conc) {
  global $r1, $r2, $r3;
  $zero_count = 0;
  if ($electron_conc == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($hole_conc == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($intrinsic_conc == 0.0) {
  $zero_count = $zero_count + 1;
}
  if ($zero_count != 1) {
  $panic('You cannot supply more or less than 2 values');
}
  if ($electron_conc < 0.0) {
  $panic('Electron concentration cannot be negative in a semiconductor');
}
  if ($hole_conc < 0.0) {
  $panic('Hole concentration cannot be negative in a semiconductor');
}
  if ($intrinsic_conc < 0.0) {
  $panic('Intrinsic concentration cannot be negative in a semiconductor');
}
  if ($electron_conc == 0.0) {
  return ['name' => 'electron_conc', 'value' => ($intrinsic_conc * $intrinsic_conc) / $hole_conc];
}
  if ($hole_conc == 0.0) {
  return ['name' => 'hole_conc', 'value' => ($intrinsic_conc * $intrinsic_conc) / $electron_conc];
}
  if ($intrinsic_conc == 0.0) {
  return ['name' => 'intrinsic_conc', 'value' => sqrtApprox($electron_conc * $hole_conc)];
}
  return ['name' => '', 'value' => -1.0];
}
$r1 = carrier_concentration(25.0, 100.0, 0.0);
echo rtrim($r1['name'] . ', ' . _str($r1['value'])), PHP_EOL;
$r2 = carrier_concentration(0.0, 1600.0, 200.0);
echo rtrim($r2['name'] . ', ' . _str($r2['value'])), PHP_EOL;
$r3 = carrier_concentration(1000.0, 0.0, 1200.0);
echo rtrim($r3['name'] . ', ' . _str($r3['value'])), PHP_EOL;
