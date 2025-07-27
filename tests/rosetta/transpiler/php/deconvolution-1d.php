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
function listToStringInts($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str(intval($xs[$i]));
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function deconv($g, $f) {
  $h = [];
  $n = 0;
  $hn = count($g) - count($f) + 1;
  while ($n < $hn) {
  $v = $g[$n];
  $lower = 0;
  if ($n >= count($f)) {
  $lower = $n - count($f) + 1;
}
  $i = $lower;
  while ($i < $n) {
  $v = $v - $h[$i] * $f[$n - $i];
  $i = $i + 1;
};
  $v = $v / $f[0];
  $h = array_merge($h, [$v]);
  $n = $n + 1;
};
  return $h;
}
function main() {
  $h = [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0];
  $f = [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0];
  $g = [24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0];
  echo rtrim(listToStringInts($h)), PHP_EOL;
  echo rtrim(listToStringInts(deconv($g, $f))), PHP_EOL;
  echo rtrim(listToStringInts($f)), PHP_EOL;
  echo rtrim(listToStringInts(deconv($g, $h))), PHP_EOL;
}
main();
