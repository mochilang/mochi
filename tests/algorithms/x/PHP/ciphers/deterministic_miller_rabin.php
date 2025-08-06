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
function mod_pow($base, $exp, $mod) {
  $result = 1;
  $b = $base % $mod;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $mod;
}
  $b = ($b * $b) % $mod;
  $e = _intdiv($e, 2);
};
  return $result;
}
function miller_rabin($n, $allow_probable) {
  if ($n == 2) {
  return true;
}
  if ($n < 2 || $n % 2 == 0) {
  return false;
}
  if ($n > 5) {
  $last = $n % 10;
  if (!($last == 1 || $last == 3 || $last == 7 || $last == 9)) {
  return false;
};
}
  $limit = 3825123056546413051;
  if ($n > $limit && (!$allow_probable)) {
  $panic('Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test.');
}
  $bounds = [2047, 1373653, 25326001, 3215031751, 2152302898747, 3474749660383, 341550071728321, $limit];
  $primes = [2, 3, 5, 7, 11, 13, 17, 19];
  $i = 0;
  $plist_len = count($primes);
  while ($i < count($bounds)) {
  if ($n < $bounds[$i]) {
  $plist_len = $i + 1;
  $i = count($bounds);
} else {
  $i = $i + 1;
}
};
  $d = $n - 1;
  $s = 0;
  while ($d % 2 == 0) {
  $d = _intdiv($d, 2);
  $s = $s + 1;
};
  $j = 0;
  while ($j < $plist_len) {
  $prime = $primes[$j];
  $x = mod_pow($prime, $d, $n);
  $pr = false;
  if ($x == 1 || $x == $n - 1) {
  $pr = true;
} else {
  $r = 1;
  while ($r < $s && (!$pr)) {
  $x = ($x * $x) % $n;
  if ($x == $n - 1) {
  $pr = true;
}
  $r = $r + 1;
};
}
  if (!$pr) {
  return false;
}
  $j = $j + 1;
};
  return true;
}
echo rtrim(_str(miller_rabin(561, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(563, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(838201, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(838207, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(17316001, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(17316017, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(3078386641, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(3078386653, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(1713045574801, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(1713045574819, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(2779799728307, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(2779799728327, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(113850023909441, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(113850023909527, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(1275041018848804351, false))), PHP_EOL;
echo rtrim(_str(miller_rabin(1275041018848804391, false))), PHP_EOL;
