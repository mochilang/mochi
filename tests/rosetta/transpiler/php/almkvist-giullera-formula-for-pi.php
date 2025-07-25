<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
function repeat($s, $n) {
    return str_repeat($s, intval($n));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : sprintf('%.0f', $a);
        $sb = is_int($b) ? strval($b) : sprintf('%.0f', $b);
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function bigTrim($a) {
  $n = count($a);
  while ($n > 1 && $a[$n - 1] == 0) {
  $a = array_slice($a, 0, $n - 1 - 0);
  $n = $n - 1;
};
  return $a;
};
  function bigFromInt($x) {
  if ($x == 0) {
  return [0];
}
  $digits = [];
  $n = $x;
  while ($n > 0) {
  $digits = array_merge($digits, [$n % 10]);
  $n = _intdiv($n, 10);
};
  return $digits;
};
  function bigCmp($a, $b) {
  if (count($a) > count($b)) {
  return 1;
}
  if (count($a) < count($b)) {
  return -1;
}
  $i = count($a) - 1;
  while ($i >= 0) {
  if ($a[$i] > $b[$i]) {
  return 1;
}
  if ($a[$i] < $b[$i]) {
  return -1;
}
  $i = $i - 1;
};
  return 0;
};
  function bigAdd($a, $b) {
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a) || $i < count($b) || $carry > 0) {
  $av = 0;
  if ($i < count($a)) {
  $av = $a[$i];
}
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $s = $av + $bv + $carry;
  $res = array_merge($res, [$s % 10]);
  $carry = _intdiv($s, 10);
  $i = $i + 1;
};
  return bigTrim($res);
};
  function bigSub($a, $b) {
  $res = [];
  $borrow = 0;
  $i = 0;
  while ($i < count($a)) {
  $av = $a[$i];
  $bv = 0;
  if ($i < count($b)) {
  $bv = $b[$i];
}
  $diff = $av - $bv - $borrow;
  if ($diff < 0) {
  $diff = $diff + 10;
  $borrow = 1;
} else {
  $borrow = 0;
}
  $res = array_merge($res, [$diff]);
  $i = $i + 1;
};
  return bigTrim($res);
};
  function bigMulSmall($a, $m) {
  if ($m == 0) {
  return [0];
}
  $res = [];
  $carry = 0;
  $i = 0;
  while ($i < count($a)) {
  $prod = $a[$i] * $m + $carry;
  $res = array_merge($res, [$prod % 10]);
  $carry = _intdiv($prod, 10);
  $i = $i + 1;
};
  while ($carry > 0) {
  $res = array_merge($res, [$carry % 10]);
  $carry = _intdiv($carry, 10);
};
  return bigTrim($res);
};
  function bigMulBig($a, $b) {
  $res = [];
  $i = 0;
  while ($i < count($a) + count($b)) {
  $res = array_merge($res, [0]);
  $i = $i + 1;
};
  $i = 0;
  while ($i < count($a)) {
  $carry = 0;
  $j = 0;
  while ($j < count($b)) {
  $idx = $i + $j;
  $prod = $res[$idx] + $a[$i] * $b[$j] + $carry;
  $res[$idx] = $prod % 10;
  $carry = _intdiv($prod, 10);
  $j = $j + 1;
};
  $idx = $i + count($b);
  while ($carry > 0) {
  $prod = $res[$idx] + $carry;
  $res[$idx] = $prod % 10;
  $carry = _intdiv($prod, 10);
  $idx = $idx + 1;
};
  $i = $i + 1;
};
  return bigTrim($res);
};
  function bigMulPow10($a, $k) {
  $i = 0;
  while ($i < $k) {
  $a = array_merge([0], $a);
  $i = $i + 1;
};
  return $a;
};
  function bigDivSmall($a, $m) {
  $res = [];
  $rem = 0;
  $i = count($a) - 1;
  while ($i >= 0) {
  $cur = $rem * 10 + $a[$i];
  $q = _intdiv($cur, $m);
  $rem = $cur % $m;
  $res = array_merge([$q], $res);
  $i = $i - 1;
};
  return bigTrim($res);
};
  function bigToString($a) {
  $s = '';
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . _str($a[$i]);
  $i = $i - 1;
};
  return $s;
};
  function mochi_repeat($ch, $n) {
  $s = '';
  $i = 0;
  while ($i < $n) {
  $s = $s . $ch;
  $i = $i + 1;
};
  return $s;
};
  function sortInts($xs) {
  $res = [];
  $tmp = $xs;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $i = 1;
  while ($i < count($tmp)) {
  if ($tmp[$i] < $min) {
  $min = $tmp[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $j = 0;
  while ($j < count($tmp)) {
  if ($j != $idx) {
  $out = array_merge($out, [$tmp[$j]]);
}
  $j = $j + 1;
};
  $tmp = $out;
};
  return $res;
};
  function primesUpTo($n) {
  $sieve = [];
  $i = 0;
  while ($i <= $n) {
  $sieve = array_merge($sieve, [true]);
  $i = $i + 1;
};
  $p = 2;
  while ($p * $p <= $n) {
  if ($sieve[$p]) {
  $m = $p * $p;
  while ($m <= $n) {
  $sieve[$m] = false;
  $m = $m + $p;
};
}
  $p = $p + 1;
};
  $res = [];
  $x = 2;
  while ($x <= $n) {
  if ($sieve[$x]) {
  $res = array_merge($res, [$x]);
}
  $x = $x + 1;
};
  return $res;
};
  function factorialExp($n, $primes) {
  $m = [];
  foreach ($primes as $p) {
  if ($p > $n) {
  break;
}
  $t = $n;
  $e = 0;
  while ($t > 0) {
  $t = _intdiv($t, $p);
  $e = $e + $t;
};
  $m[_str($p)] = $e;
};
  return $m;
};
  function factorSmall($x, $primes) {
  $f = [];
  $n = $x;
  foreach ($primes as $p) {
  if ($p * $p > $n) {
  break;
}
  $c = 0;
  while ($n % $p == 0) {
  $c = $c + 1;
  $n = _intdiv($n, $p);
};
  if ($c > 0) {
  $f[_str($p)] = $c;
}
};
  if ($n > 1) {
  $f[_str($n)] = (array_key_exists(_str($n), $f) ? $f[_str($n)] : 0) + 1;
}
  return $f;
};
  function computeIP($n, $primes) {
  $exps = factorialExp(6 * $n, $primes);
  $fn = factorialExp($n, $primes);
  foreach (array_keys($fn) as $k) {
  $exps[$k] = (array_key_exists($k, $exps) ? $exps[$k] : 0) - 6 * $fn[$k];
};
  $exps['2'] = (array_key_exists('2', $exps) ? $exps['2'] : 0) + 5;
  $t2 = 532 * $n * $n + 126 * $n + 9;
  $ft2 = factorSmall($t2, $primes);
  foreach (array_keys($ft2) as $k) {
  $exps[$k] = (array_key_exists($k, $exps) ? $exps[$k] : 0) + $ft2[$k];
};
  $exps['3'] = (array_key_exists('3', $exps) ? $exps['3'] : 0) - 1;
  $keys = [];
  foreach (array_keys($exps) as $k) {
  $keys = array_merge($keys, [intval($k)]);
};
  $keys = sortInts($keys);
  $res = bigFromInt(1);
  foreach ($keys as $p) {
  $e = $exps[_str($p)];
  $i = 0;
  while ($i < $e) {
  $res = bigMulSmall($res, $p);
  $i = $i + 1;
};
};
  return $res;
};
  function formatTerm($ip, $pw) {
  $s = bigToString($ip);
  if ($pw >= strlen($s)) {
  $frac = repeat('0', $pw - strlen($s)) . $s;
  if (strlen($frac) < 33) {
  $frac = $frac . repeat('0', 33 - strlen($frac));
};
  return '0.' . substr($frac, 0, 33 - 0);
}
  $intpart = substr($s, 0, strlen($s) - $pw - 0);
  $frac = substr($s, strlen($s) - $pw, strlen($s) - (strlen($s) - $pw));
  if (strlen($frac) < 33) {
  $frac = $frac . repeat('0', 33 - strlen($frac));
}
  return $intpart . '.' . substr($frac, 0, 33 - 0);
};
  function bigAbsDiff($a, $b) {
  if (bigCmp($a, $b) >= 0) {
  return bigSub($a, $b);
}
  return bigSub($b, $a);
};
  function main() {
  $primes = primesUpTo(2000);
  echo rtrim('N                               Integer Portion  Pow  Nth Term (33 dp)'), PHP_EOL;
  $line = repeat('-', 89);
  echo rtrim($line), PHP_EOL;
  $sum = bigFromInt(0);
  $prev = bigFromInt(0);
  $denomPow = 0;
  $n = 0;
  while (true) {
  $ip = computeIP($n, $primes);
  $pw = 6 * $n + 3;
  if ($pw > $denomPow) {
  $sum = bigMulPow10($sum, $pw - $denomPow);
  $prev = bigMulPow10($prev, $pw - $denomPow);
  $denomPow = $pw;
}
  if ($n < 10) {
  $termStr = formatTerm($ip, $pw);
  $ipStr = bigToString($ip);
  while (strlen($ipStr) < 44) {
  $ipStr = ' ' . $ipStr;
};
  $pwStr = _str(-$pw);
  while (strlen($pwStr) < 3) {
  $pwStr = ' ' . $pwStr;
};
  $padTerm = $termStr;
  while (strlen($padTerm) < 35) {
  $padTerm = $padTerm . ' ';
};
  echo rtrim(_str($n) . '  ' . $ipStr . '  ' . $pwStr . '  ' . $padTerm), PHP_EOL;
}
  $sum = bigAdd($sum, $ip);
  $diff = bigAbsDiff($sum, $prev);
  if ($denomPow >= 70 && bigCmp($diff, bigMulPow10(bigFromInt(1), $denomPow - 70)) < 0) {
  break;
}
  $prev = $sum;
  $n = $n + 1;
};
  $precision = 70;
  $target = bigMulPow10(bigFromInt(1), $denomPow + 2 * $precision);
  $low = bigFromInt(0);
  $high = bigMulPow10(bigFromInt(1), $precision + 1);
  while (bigCmp($low, bigSub($high, bigFromInt(1))) < 0) {
  $mid = bigDivSmall(bigAdd($low, $high), 2);
  $prod = bigMulBig(bigMulBig($mid, $mid), $sum);
  if (bigCmp($prod, $target) <= 0) {
  $low = $mid;
} else {
  $high = bigSub($mid, bigFromInt(1));
}
};
  $piInt = $low;
  $piStr = bigToString($piInt);
  if (strlen($piStr) <= $precision) {
  $piStr = repeat('0', $precision - strlen($piStr) + 1) . $piStr;
}
  $out = substr($piStr, 0, strlen($piStr) - $precision - 0) . '.' . substr($piStr, strlen($piStr) - $precision, strlen($piStr) - (strlen($piStr) - $precision));
  echo rtrim(''), PHP_EOL;
  echo rtrim('Pi to 70 decimal places is:'), PHP_EOL;
  echo rtrim($out), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
