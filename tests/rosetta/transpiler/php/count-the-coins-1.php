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
function countChange($amount) {
  return cc($amount, 4);
}
function cc($amount, $kindsOfCoins) {
  if ($amount == 0) {
  return 1;
}
  if ($amount < 0 || $kindsOfCoins == 0) {
  return 0;
}
  return cc($amount, $kindsOfCoins - 1) + cc($amount - firstDenomination($kindsOfCoins), $kindsOfCoins);
}
function firstDenomination($kindsOfCoins) {
  global $amount;
  if ($kindsOfCoins == 1) {
  return 1;
}
  if ($kindsOfCoins == 2) {
  return 5;
}
  if ($kindsOfCoins == 3) {
  return 10;
}
  if ($kindsOfCoins == 4) {
  return 25;
}
  return 0;
}
$amount = 100;
echo rtrim('amount, ways to make change: ' . _str($amount) . ' ' . _str(countChange($amount))), PHP_EOL;
