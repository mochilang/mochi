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
function egcd($a, $b) {
  global $modInv, $crt, $n;
  if ($a == 0) {
  return [$b, 0, 1];
}
  $res = egcd($b % $a, $a);
  $g = $res[0];
  $x1 = $res[1];
  $y1 = $res[2];
  return [$g, $y1 - (intdiv($b, $a)) * $x1, $x1];
}
function modInv($a, $m) {
  global $egcd, $crt, $n, $res;
  $r = egcd($a, $m);
  if ($r[0] != 1) {
  return 0;
}
  $x = $r[1];
  if ($x < 0) {
  return $x + $m;
}
  return $x;
}
function crt($a, $n) {
  global $egcd, $modInv, $res;
  $prod = 1;
  $i = 0;
  while ($i < count($n)) {
  $prod = $prod * $n[$i];
  $i = $i + 1;
};
  $x = 0;
  $i = 0;
  while ($i < count($n)) {
  $ni = $n[$i];
  $ai = $a[$i];
  $p = intdiv($prod, $ni);
  $inv = modInv($p % $ni, $ni);
  $x = $x + $ai * $inv * $p;
  $i = $i + 1;
};
  return $x % $prod;
}
$n = [3, 5, 7];
$a = [2, 3, 2];
$res = crt($a, $n);
echo rtrim(_str($res) . ' <nil>'), PHP_EOL;
