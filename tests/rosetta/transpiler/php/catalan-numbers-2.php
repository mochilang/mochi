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
function catalanRec($n) {
  global $main;
  if ($n == 0) {
  return 1;
}
  $t1 = 2 * $n;
  $t2 = $t1 - 1;
  $t3 = 2 * $t2;
  $t5 = $t3 * catalanRec($n - 1);
  return intval((intdiv($t5, ($n + 1))));
}
function main() {
  global $catalanRec;
  for ($i = 1; $i < 16; $i++) {
  echo rtrim(json_encode(_str(catalanRec($i)), 1344)), PHP_EOL;
};
}
main();
