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
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function modPow($base, $exp, $m) {
  $result = 1 % $m;
  $b = $base % $m;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $m;
}
  $b = ($b * $b) % $m;
  $e = intval((_intdiv($e, 2)));
};
  return $result;
}
function isPrime($n) {
  if ($n < 2) {
  return false;
}
  foreach ([2, 3, 5, 7, 11, 13, 17, 19, 23, 29] as $p) {
  if ($n % $p == 0) {
  return $n == $p;
}
};
  $d = $n - 1;
  $s = 0;
  while ($d % 2 == 0) {
  $d = _intdiv($d, 2);
  $s = $s + 1;
};
  foreach ([2, 325, 9375, 28178, 450775, 9780504, 1795265022] as $a) {
  if ($a % $n == 0) {
  return true;
}
  $x = modPow($a, $d, $n);
  if ($x == 1 || $x == $n - 1) {
  continue;
}
  $r = 1;
  $passed = false;
  while ($r < $s) {
  $x = ($x * $x) % $n;
  if ($x == $n - 1) {
  $passed = true;
  break;
}
  $r = $r + 1;
};
  if (!$passed) {
  return false;
}
};
  return true;
}
function commatize($n) {
  $s = _str($n);
  $i = strlen($s) - 3;
  while ($i > 0) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  return $s;
}
function pad($s, $width) {
  $out = $s;
  while (strlen($out) < $width) {
  $out = ' ' . $out;
};
  return $out;
}
function mochi_join($xs, $sep) {
  $res = '';
  $i = 0;
  while ($i < count($xs)) {
  if ($i > 0) {
  $res = $res . $sep;
}
  $res = $res . $xs[$i];
  $i = $i + 1;
};
  return $res;
}
function formatRow($row) {
  $padded = [];
  $i = 0;
  while ($i < count($row)) {
  $padded = array_merge($padded, [pad($row[$i], 9)]);
  $i = $i + 1;
};
  return '[' . mochi_join($padded, ' ') . ']';
}
function main() {
  $cubans = [];
  $cube1 = 1;
  $count = 0;
  $cube100k = 0;
  $i = 1;
  while (true) {
  $j = $i + 1;
  $cube2 = $j * $j * $j;
  $diff = $cube2 - $cube1;
  if (isPrime($diff)) {
  if ($count < 200) {
  $cubans = array_merge($cubans, [commatize($diff)]);
};
  $count = $count + 1;
  if ($count == 100000) {
  $cube100k = $diff;
  break;
};
}
  $cube1 = $cube2;
  $i = $i + 1;
};
  echo rtrim('The first 200 cuban primes are:-'), PHP_EOL;
  $row = 0;
  while ($row < 20) {
  $slice = [];
  $k = 0;
  while ($k < 10) {
  $slice = array_merge($slice, [$cubans[$row * 10 + $k]]);
  $k = $k + 1;
};
  echo rtrim(formatRow($slice)), PHP_EOL;
  $row = $row + 1;
};
  echo rtrim('
The 100,000th cuban prime is ' . commatize($cube100k)), PHP_EOL;
}
main();
