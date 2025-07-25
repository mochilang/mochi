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
  $ways = [];
  $i = 0;
  while ($i <= $amount) {
  $ways = array_merge($ways, [0]);
  $i = $i + 1;
};
  $ways[0] = 1;
  foreach ([100, 50, 25, 10, 5, 1] as $coin) {
  $j = $coin;
  while ($j <= $amount) {
  $ways[$j] = $ways[$j] + $ways[$j - $coin];
  $j = $j + 1;
};
};
  return $ways[$amount];
}
$amount = 1000 * 100;
echo rtrim('amount, ways to make change: ' . _str($amount) . ' ' . _str(countChange($amount))), PHP_EOL;
