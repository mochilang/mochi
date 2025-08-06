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
function sqrt($x) {
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
function real_power($apparent_power, $power_factor) {
  if ($power_factor < 0.0 - 1.0 || $power_factor > 1.0) {
  $panic('power_factor must be a valid float value between -1 and 1.');
}
  return $apparent_power * $power_factor;
}
function reactive_power($apparent_power, $power_factor) {
  if ($power_factor < 0.0 - 1.0 || $power_factor > 1.0) {
  $panic('power_factor must be a valid float value between -1 and 1.');
}
  return $apparent_power * sqrt(1.0 - $power_factor * $power_factor);
}
echo rtrim(_str(real_power(100.0, 0.9))), PHP_EOL;
echo rtrim(_str(real_power(0.0, 0.8))), PHP_EOL;
echo rtrim(_str(real_power(100.0, -0.9))), PHP_EOL;
echo rtrim(_str(reactive_power(100.0, 0.9))), PHP_EOL;
echo rtrim(_str(reactive_power(0.0, 0.8))), PHP_EOL;
echo rtrim(_str(reactive_power(100.0, -0.9))), PHP_EOL;
