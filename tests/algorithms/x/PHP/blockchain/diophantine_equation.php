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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function gcd($a, $b) {
  global $s1, $sols, $j;
  $x = ($a < 0 ? -$a : $a);
  $y = ($b < 0 ? -$b : $b);
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
}
function extended_gcd($a, $b) {
  global $s1, $sols, $j;
  if ($b == 0) {
  return [$a, 1, 0];
}
  $res = extended_gcd($b, $a % $b);
  $d = $res[0];
  $p = $res[1];
  $q = $res[2];
  $x = $q;
  $y = $p - $q * (_intdiv($a, $b));
  return [$d, $x, $y];
}
function diophantine($a, $b, $c) {
  global $s1, $sols, $j;
  $d = gcd($a, $b);
  if ($c % $d != 0) {
  $panic('No solution');
}
  $eg = extended_gcd($a, $b);
  $r = _intdiv($c, $d);
  $x = $eg[1] * $r;
  $y = $eg[2] * $r;
  return [$x, $y];
}
function diophantine_all_soln($a, $b, $c, $n) {
  global $s1, $j;
  $base = diophantine($a, $b, $c);
  $x0 = $base[0];
  $y0 = $base[1];
  $d = gcd($a, $b);
  $p = _intdiv($a, $d);
  $q = _intdiv($b, $d);
  $sols = [];
  $i = 0;
  while ($i < $n) {
  $x = $x0 + $i * $q;
  $y = $y0 - $i * $p;
  $sols = _append($sols, [$x, $y]);
  $i = $i + 1;
};
  return $sols;
}
$s1 = diophantine(10, 6, 14);
echo rtrim(_str($s1)), PHP_EOL;
$sols = diophantine_all_soln(10, 6, 14, 4);
$j = 0;
while ($j < count($sols)) {
  echo rtrim(_str($sols[$j])), PHP_EOL;
  $j = $j + 1;
}
echo rtrim(_str(diophantine(391, 299, -69))), PHP_EOL;
echo rtrim(_str(extended_gcd(10, 6))), PHP_EOL;
echo rtrim(_str(extended_gcd(7, 5))), PHP_EOL;
