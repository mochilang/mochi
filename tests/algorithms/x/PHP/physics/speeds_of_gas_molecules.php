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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$PI = 3.141592653589793;
$R = 8.31446261815324;
function mochi_sqrt($x) {
  global $PI, $R;
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
function avg_speed_of_molecule($temperature, $molar_mass) {
  global $PI, $R;
  if ($temperature < 0.0) {
  _panic('Absolute temperature cannot be less than 0 K');
}
  if ($molar_mass <= 0.0) {
  _panic('Molar mass should be greater than 0 kg/mol');
}
  $expr = 8.0 * $R * $temperature / ($PI * $molar_mass);
  $s = mochi_sqrt($expr);
  return $s;
}
function mps_speed_of_molecule($temperature, $molar_mass) {
  global $PI, $R;
  if ($temperature < 0.0) {
  _panic('Absolute temperature cannot be less than 0 K');
}
  if ($molar_mass <= 0.0) {
  _panic('Molar mass should be greater than 0 kg/mol');
}
  $expr = 2.0 * $R * $temperature / $molar_mass;
  $s = mochi_sqrt($expr);
  return $s;
}
echo rtrim(_str(avg_speed_of_molecule(273.0, 0.028))), PHP_EOL;
echo rtrim(_str(avg_speed_of_molecule(300.0, 0.032))), PHP_EOL;
echo rtrim(_str(mps_speed_of_molecule(273.0, 0.028))), PHP_EOL;
echo rtrim(_str(mps_speed_of_molecule(300.0, 0.032))), PHP_EOL;
