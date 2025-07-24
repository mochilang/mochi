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
function binom($n, $k) {
  global $catalan, $main;
  if ($k < 0 || $k > $n) {
  return 0;
}
  $kk = $k;
  if ($kk > $n - $kk) {
  $kk = $n - $kk;
}
  $res = 1;
  $i = 0;
  while ($i < $kk) {
  $res = ($res * ($n - $i));
  $i = $i + 1;
  $res = intval((intdiv($res, $i)));
};
  return $res;
}
function catalan($n) {
  global $binom, $main;
  return intval((binom(2 * $n, $n) / ($n + 1)));
}
function main() {
  global $binom, $catalan;
  for ($i = 0; $i < 15; $i++) {
  echo rtrim(json_encode(_str(catalan($i)), 1344)), PHP_EOL;
};
}
main();
