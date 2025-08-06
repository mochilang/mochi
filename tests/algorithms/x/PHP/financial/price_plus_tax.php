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
function price_plus_tax($price, $tax_rate) {
  return $price * (1.0 + $tax_rate);
}
echo rtrim('price_plus_tax(100, 0.25) = ' . _str(price_plus_tax(100.0, 0.25))), PHP_EOL;
echo rtrim('price_plus_tax(125.50, 0.05) = ' . _str(price_plus_tax(125.5, 0.05))), PHP_EOL;
