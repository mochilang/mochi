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
function pow_float($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
}
function equated_monthly_installments($principal, $rate_per_annum, $years_to_repay) {
  if ($principal <= 0.0) {
  $panic('Principal borrowed must be > 0');
}
  if ($rate_per_annum < 0.0) {
  $panic('Rate of interest must be >= 0');
}
  if ($years_to_repay <= 0) {
  $panic('Years to repay must be an integer > 0');
}
  $rate_per_month = $rate_per_annum / 12.0;
  $number_of_payments = $years_to_repay * 12;
  $factor = pow_float(1.0 + $rate_per_month, $number_of_payments);
  return $principal * $rate_per_month * $factor / ($factor - 1.0);
}
echo rtrim(_str(equated_monthly_installments(25000.0, 0.12, 3))), PHP_EOL;
echo rtrim(_str(equated_monthly_installments(25000.0, 0.12, 10))), PHP_EOL;
