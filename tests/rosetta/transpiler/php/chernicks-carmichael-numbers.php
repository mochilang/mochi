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
function isPrime($n) {
  global $bigTrim, $bigFromInt, $bigMulSmall, $bigToString, $pow2, $ccFactors, $ccNumbers;
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
}
function bigTrim($a) {
  global $isPrime, $bigFromInt, $bigMulSmall, $bigToString, $pow2, $ccFactors, $ccNumbers;
  $n = count($a);
  while ($n > 1 && $a[$n - 1] == 0) {
  $a = array_slice($a, 0, $n - 1 - 0);
  $n = $n - 1;
};
  return $a;
}
function bigFromInt($x) {
  global $isPrime, $bigTrim, $bigMulSmall, $bigToString, $pow2, $ccFactors, $ccNumbers;
  if ($x == 0) {
  return [0];
}
  $digits = [];
  $n = $x;
  while ($n > 0) {
  $digits = array_merge($digits, [$n % 10]);
  $n = intdiv($n, 10);
};
  return $digits;
}
function bigMulSmall($a, $m) {
  global $isPrime, $bigTrim, $bigFromInt, $bigToString, $pow2, $ccFactors, $ccNumbers;
  if ($m == 0) {
  return [0];
}
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a)) {
  $prod = $a[$i] * $m + $carry;
  $res = array_merge($res, [$prod % 10]);
  $carry = intdiv($prod, 10);
  $i = $i + 1;
};
  while ($carry > 0) {
  $res = array_merge($res, [$carry % 10]);
  $carry = intdiv($carry, 10);
};
  return bigTrim($res);
}
function bigToString($a) {
  global $isPrime, $bigTrim, $bigFromInt, $bigMulSmall, $pow2, $ccFactors, $ccNumbers;
  $s = '';
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . _str($a[$i]);
  $i = $i - 1;
};
  return $s;
}
function pow2($k) {
  global $isPrime, $bigTrim, $bigFromInt, $bigMulSmall, $bigToString, $ccFactors, $ccNumbers;
  $r = 1;
  $i = 0;
  while ($i < $k) {
  $r = $r * 2;
  $i = $i + 1;
};
  return $r;
}
function ccFactors($n, $m) {
  global $isPrime, $bigTrim, $bigFromInt, $bigMulSmall, $bigToString, $pow2, $ccNumbers;
  $p = 6 * $m + 1;
  if (!isPrime($p)) {
  return [];
}
  $prod = bigFromInt($p);
  $p = 12 * $m + 1;
  if (!isPrime($p)) {
  return [];
}
  $prod = bigMulSmall($prod, $p);
  $i = 1;
  while ($i <= $n - 2) {
  $p = (pow2($i) * 9 * $m) + 1;
  if (!isPrime($p)) {
  return [];
}
  $prod = bigMulSmall($prod, $p);
  $i = $i + 1;
};
  return $prod;
}
function ccNumbers($start, $end) {
  global $isPrime, $bigTrim, $bigFromInt, $bigMulSmall, $bigToString, $pow2, $ccFactors;
  $n = $start;
  while ($n <= $end) {
  $m = 1;
  if ($n > 4) {
  $m = pow2($n - 4);
}
  while (true) {
  $num = ccFactors($n, $m);
  if (count($num) > 0) {
  echo rtrim('a(' . _str($n) . ') = ' . bigToString($num)), PHP_EOL;
  break;
}
  if ($n <= 4) {
  $m = $m + 1;
} else {
  $m = $m + pow2($n - 4);
}
};
  $n = $n + 1;
};
}
ccNumbers(3, 9);
