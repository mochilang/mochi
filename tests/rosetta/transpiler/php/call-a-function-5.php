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
function doIt($p) {
  global $main;
  $b = 0;
  if (array_key_exists('b', $p)) {
  $b = $p['b'];
}
  return $p['a'] + $b + $p['c'];
}
function main() {
  global $doIt;
  $p = [];
  $p['a'] = 1;
  $p['c'] = 9;
  echo rtrim(json_encode(_str(doIt($p)), 1344)), PHP_EOL;
}
main();
