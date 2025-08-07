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
$G = 9.80665;
function mochi_sqrt($x) {
  global $G;
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
function terminal_velocity($mass, $density, $area, $drag_coefficient) {
  global $G;
  if ($mass <= 0.0 || $density <= 0.0 || $area <= 0.0 || $drag_coefficient <= 0.0) {
  _panic('mass, density, area and the drag coefficient all need to be positive');
}
  $numerator = 2.0 * $mass * $G;
  $denominator = $density * $area * $drag_coefficient;
  $result = mochi_sqrt($numerator / $denominator);
  return $result;
}
echo rtrim(_str(terminal_velocity(1.0, 25.0, 0.6, 0.77))), PHP_EOL;
echo rtrim(_str(terminal_velocity(2.0, 100.0, 0.45, 0.23))), PHP_EOL;
echo rtrim(_str(terminal_velocity(5.0, 50.0, 0.2, 0.5))), PHP_EOL;
