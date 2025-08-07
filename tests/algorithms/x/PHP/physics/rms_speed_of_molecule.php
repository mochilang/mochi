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
$UNIVERSAL_GAS_CONSTANT = 8.3144598;
function mochi_sqrt($x) {
  global $UNIVERSAL_GAS_CONSTANT, $vrms;
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
function rms_speed_of_molecule($temperature, $molar_mass) {
  global $UNIVERSAL_GAS_CONSTANT, $vrms;
  if ($temperature < 0.0) {
  _panic('Temperature cannot be less than 0 K');
}
  if ($molar_mass <= 0.0) {
  _panic('Molar mass cannot be less than or equal to 0 kg/mol');
}
  $num = 3.0 * $UNIVERSAL_GAS_CONSTANT * $temperature;
  $val = $num / $molar_mass;
  $result = mochi_sqrt($val);
  return $result;
}
echo rtrim('rms_speed_of_molecule(100, 2) = ' . _str(rms_speed_of_molecule(100.0, 2.0))), PHP_EOL;
echo rtrim('rms_speed_of_molecule(273, 12) = ' . _str(rms_speed_of_molecule(273.0, 12.0))), PHP_EOL;
$vrms = rms_speed_of_molecule(300.0, 28.0);
echo rtrim('Vrms of Nitrogen gas at 300 K is ' . _str($vrms) . ' m/s'), PHP_EOL;
