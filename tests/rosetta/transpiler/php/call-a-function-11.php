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
function zeroval($ival) {
  global $zeroptr, $main;
  $x = $ival;
  $x = 0;
  return $x;
}
function zeroptr(&$ref) {
  global $zeroval, $main;
  $ref[0] = 0;
}
function main() {
  global $zeroval, $zeroptr;
  $i = 1;
  echo rtrim('initial: ' . _str($i)), PHP_EOL;
  $tmp = zeroval($i);
  echo rtrim('zeroval: ' . _str($i)), PHP_EOL;
  $box = [$i];
  zeroptr($box);
  $i = $box[0];
  echo rtrim('zeroptr: ' . _str($i)), PHP_EOL;
  echo rtrim('pointer: 0'), PHP_EOL;
}
main();
