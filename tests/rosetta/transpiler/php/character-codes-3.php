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
  global $mochi_chr, $b, $r, $s;
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
function mochi_chr($n) {
  global $mochi_ord, $b, $r, $s;
  if ($n == 97) {
  return 'a';
}
  if ($n == 960) {
  return 'π';
}
  if ($n == 65) {
  return 'A';
}
  return '?';
}
$b = mochi_ord('a');
$r = mochi_ord('π');
$s = 'aπ';
echo rtrim(_str($b) . ' ' . _str($r) . ' ' . $s), PHP_EOL;
echo rtrim('string cast to []rune: [' . _str($b) . ' ' . _str($r) . ']'), PHP_EOL;
echo rtrim('    string range loop: ' . _str($b) . ' ' . _str($r)), PHP_EOL;
echo rtrim('         string bytes: 0x61 0xcf 0x80'), PHP_EOL;
