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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function panic($msg) {
  echo rtrim($msg), PHP_EOL;
}
function catalan_numbers($upper_limit) {
  if ($upper_limit < 0) {
  panic('Limit for the Catalan sequence must be >= 0');
  return [];
}
  $catalans = [1];
  $n = 1;
  while ($n <= $upper_limit) {
  $next_val = 0;
  $j = 0;
  while ($j < $n) {
  $next_val = $next_val + $catalans[$j] * $catalans[$n - $j - 1];
  $j = $j + 1;
};
  $catalans = _append($catalans, $next_val);
  $n = $n + 1;
};
  return $catalans;
}
echo rtrim(_str(catalan_numbers(5))), PHP_EOL;
echo rtrim(_str(catalan_numbers(2))), PHP_EOL;
