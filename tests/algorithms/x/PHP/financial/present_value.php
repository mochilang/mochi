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
function powf($base, $exponent) {
  $result = 1.0;
  $i = 0;
  while ($i < $exponent) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function round2($value) {
  if ($value >= 0.0) {
  $scaled = intval(($value * 100.0 + 0.5));
  return (floatval($scaled)) / 100.0;
}
  $scaled = intval(($value * 100.0 - 0.5));
  return (floatval($scaled)) / 100.0;
}
function present_value($discount_rate, $cash_flows) {
  if ($discount_rate < 0.0) {
  $panic('Discount rate cannot be negative');
}
  if (count($cash_flows) == 0) {
  $panic('Cash flows list cannot be empty');
}
  $pv = 0.0;
  $i = 0;
  $factor = 1.0 + $discount_rate;
  while ($i < count($cash_flows)) {
  $cf = $cash_flows[$i];
  $pv = $pv + $cf / powf($factor, $i);
  $i = $i + 1;
};
  return round2($pv);
}
echo rtrim(_str(present_value(0.13, [10.0, 20.7, -293.0, 297.0]))), PHP_EOL;
echo rtrim(_str(present_value(0.07, [-109129.39, 30923.23, 15098.93, 29734.0, 39.0]))), PHP_EOL;
echo rtrim(_str(present_value(0.07, [109129.39, 30923.23, 15098.93, 29734.0, 39.0]))), PHP_EOL;
