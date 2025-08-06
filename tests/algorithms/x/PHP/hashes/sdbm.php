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
$ascii = ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~';
function mochi_ord($ch) {
  global $ascii;
  $i = 0;
  while ($i < strlen($ascii)) {
  if (substr($ascii, $i, $i + 1 - $i) == $ch) {
  return 32 + $i;
}
  $i = $i + 1;
};
  return 0;
}
function sdbm($plain_text) {
  global $ascii;
  $hash_value = 0;
  $i = 0;
  while ($i < strlen($plain_text)) {
  $code = mochi_ord(substr($plain_text, $i, $i + 1 - $i));
  $hash_value = $hash_value * 65599 + $code;
  $i = $i + 1;
};
  return $hash_value;
}
echo rtrim(_str(sdbm('Algorithms'))), PHP_EOL;
echo rtrim(_str(sdbm('scramble bits'))), PHP_EOL;
