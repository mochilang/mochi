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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function indexOf($s, $ch) {
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function fmt1($x) {
  $y = floatval(intval((($x * 10.0) + 0.5))) / 10.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot < 0) {
  $s = $s . '.0';
}
  return $s;
}
function listToString1($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . fmt1($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s . ']';
}
function deconv($g, $f) {
  $out = [];
  $i = 0;
  while ($i <= count($g) - count($f)) {
  $sum = $g[$i];
  $j = 1;
  while ($j < count($f)) {
  if ($j <= $i) {
  $sum = $sum - $out[$i - $j] * $f[$j];
}
  $j = $j + 1;
};
  $out = array_merge($out, [$sum / $f[0]]);
  $i = $i + 1;
};
  return $out;
}
function main() {
  $h = [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0];
  $f = [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0];
  $g = [24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0];
  echo rtrim(listToString1($h)), PHP_EOL;
  echo rtrim(listToString1(deconv($g, $f))), PHP_EOL;
  echo rtrim(listToString1($f)), PHP_EOL;
  echo rtrim(listToString1(deconv($g, $h))), PHP_EOL;
}
main();
