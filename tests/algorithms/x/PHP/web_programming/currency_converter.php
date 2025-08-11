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
$rates = [['code' => 'USD', 'rate' => 1.0], ['code' => 'EUR', 'rate' => 0.9], ['code' => 'INR', 'rate' => 83.0], ['code' => 'JPY', 'rate' => 156.0], ['code' => 'GBP', 'rate' => 0.78]];
function rate_of($code) {
  global $rates, $result;
  foreach ($rates as $r) {
  if ($r['code'] == $code) {
  return $r['rate'];
}
};
  return 0.0;
}
function convert_currency($from_, $to, $amount) {
  global $rates, $result;
  $from_rate = rate_of($from_);
  $to_rate = rate_of($to);
  if ($from_rate == 0.0 || $to_rate == 0.0) {
  return 0.0;
}
  $usd = $amount / $from_rate;
  return $usd * $to_rate;
}
$result = convert_currency('USD', 'INR', 10.0);
echo rtrim(_str($result)), PHP_EOL;
