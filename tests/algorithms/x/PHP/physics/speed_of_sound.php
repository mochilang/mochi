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
function sqrtApprox($x) {
  if ($x == 0.0) {
  return 0.0;
}
  $guess = $x / 2.0;
  $i = 0;
  while ($i < 20) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
}
function speed_of_sound_in_a_fluid($density, $bulk_modulus) {
  if ($density <= 0.0) {
  _panic('Impossible fluid density');
}
  if ($bulk_modulus <= 0.0) {
  _panic('Impossible bulk modulus');
}
  return sqrtApprox($bulk_modulus / $density);
}
echo rtrim(_str(speed_of_sound_in_a_fluid(998.0, 2150000000.0))), PHP_EOL;
echo rtrim(_str(speed_of_sound_in_a_fluid(13600.0, 28500000000.0))), PHP_EOL;
