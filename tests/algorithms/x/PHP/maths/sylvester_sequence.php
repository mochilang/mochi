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
function sylvester($n) {
  if ($n < 1) {
  _panic('The input value of n has to be > 0');
}
  if ($n == 1) {
  return 2;
}
  $prev = sylvester($n - 1);
  $lower = $prev - 1;
  $upper = $prev;
  return $lower * $upper + 1;
}
echo rtrim(_str(sylvester(8))), PHP_EOL;
