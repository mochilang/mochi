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
function mochi_ord($ch) {
  if ($ch == 'a') {
  return 97;
}
  if ($ch == 'π') {
  return 960;
}
  if ($ch == 'A') {
  return 65;
}
  return 0;
}
echo rtrim(json_encode(_str(mochi_ord('a')), 1344)), PHP_EOL;
echo rtrim(json_encode(_str(mochi_ord('π')), 1344)), PHP_EOL;
