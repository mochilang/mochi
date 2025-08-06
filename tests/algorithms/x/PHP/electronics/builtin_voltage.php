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
function pow10($n) {
  global $BOLTZMANN, $ELECTRON_VOLT, $TEMPERATURE;
  $result = 1.0;
  $i = 0;
  while ($i < $n) {
  $result = $result * 10.0;
  $i = $i + 1;
};
  return $result;
}
$BOLTZMANN = 1.380649 / pow10(23);
$ELECTRON_VOLT = 1.602176634 / pow10(19);
$TEMPERATURE = 300.0;
function ln_series($x) {
  global $BOLTZMANN, $ELECTRON_VOLT, $TEMPERATURE;
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $n = 1;
  while ($n <= 19) {
  $sum = $sum + $term / (floatval($n));
  $term = $term * $t * $t;
  $n = $n + 2;
};
  return 2.0 * $sum;
}
function ln($x) {
  global $BOLTZMANN, $ELECTRON_VOLT, $TEMPERATURE;
  $y = $x;
  $k = 0;
  while ($y >= 10.0) {
  $y = $y / 10.0;
  $k = $k + 1;
};
  while ($y < 1.0) {
  $y = $y * 10.0;
  $k = $k - 1;
};
  return ln_series($y) + (floatval($k)) * ln_series(10.0);
}
function builtin_voltage($donor_conc, $acceptor_conc, $intrinsic_conc) {
  global $BOLTZMANN, $ELECTRON_VOLT, $TEMPERATURE;
  if ($donor_conc <= 0.0) {
  $panic('Donor concentration should be positive');
}
  if ($acceptor_conc <= 0.0) {
  $panic('Acceptor concentration should be positive');
}
  if ($intrinsic_conc <= 0.0) {
  $panic('Intrinsic concentration should be positive');
}
  if ($donor_conc <= $intrinsic_conc) {
  $panic('Donor concentration should be greater than intrinsic concentration');
}
  if ($acceptor_conc <= $intrinsic_conc) {
  $panic('Acceptor concentration should be greater than intrinsic concentration');
}
  return $BOLTZMANN * $TEMPERATURE * ln(($donor_conc * $acceptor_conc) / ($intrinsic_conc * $intrinsic_conc)) / $ELECTRON_VOLT;
}
echo rtrim(_str(builtin_voltage(pow10(17), pow10(17), pow10(10)))), PHP_EOL;
