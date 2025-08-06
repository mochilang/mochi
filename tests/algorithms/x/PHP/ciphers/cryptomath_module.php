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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function gcd($a, $b) {
  $x = ($a < 0 ? -$a : $a);
  $y = ($b < 0 ? -$b : $b);
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
}
function find_mod_inverse($a, $m) {
  if (gcd($a, $m) != 1) {
  $error('mod inverse of ' . _str($a) . ' and ' . _str($m) . ' does not exist');
}
  $u1 = 1;
  $u2 = 0;
  $u3 = $a;
  $v1 = 0;
  $v2 = 1;
  $v3 = $m;
  while ($v3 != 0) {
  $q = _intdiv($u3, $v3);
  $t1 = $u1 - $q * $v1;
  $t2 = $u2 - $q * $v2;
  $t3 = $u3 - $q * $v3;
  $u1 = $v1;
  $u2 = $v2;
  $u3 = $v3;
  $v1 = $t1;
  $v2 = $t2;
  $v3 = $t3;
};
  $res = $u1 % $m;
  if ($res < 0) {
  $res = $res + $m;
}
  return $res;
}
echo rtrim(_str(find_mod_inverse(3, 11))), PHP_EOL;
echo rtrim(_str(find_mod_inverse(7, 26))), PHP_EOL;
echo rtrim(_str(find_mod_inverse(11, 26))), PHP_EOL;
echo rtrim(_str(find_mod_inverse(17, 43))), PHP_EOL;
