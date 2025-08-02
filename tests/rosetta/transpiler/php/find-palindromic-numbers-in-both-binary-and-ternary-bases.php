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
function parseIntStr($s, $base = 10) {
    return intval($s, intval($base));
}
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function toBase($n, $b) {
  if ($n == 0) {
  return '0';
}
  $s = '';
  $x = $n;
  while ($x > 0) {
  $s = _str($x % $b) . $s;
  $x = intval((_intdiv($x, $b)));
};
  return $s;
};
  function mochi_parseIntStr($str) {
  $i = 0;
  $neg = false;
  if (strlen($str) > 0 && substr($str, 0, 0 + 1 - 0) == '-') {
  $neg = true;
  $i = 1;
}
  $n = 0;
  while ($i < strlen($str)) {
  $n = $n * 10 + (ord(substr($str, $i, $i + 1 - $i))) - (intval('0'));
  $i = $i + 1;
};
  if ($neg) {
  $n = -$n;
}
  return $n;
};
  function parseIntBase($s, $b) {
  $n = 0;
  $i = 0;
  while ($i < strlen($s)) {
  $n = $n * $b + parseIntStr(substr($s, $i, $i + 1 - $i), 10);
  $i = $i + 1;
};
  return $n;
};
  function reverseStr($s) {
  $out = '';
  $i = strlen($s) - 1;
  while ($i >= 0) {
  $out = $out . substr($s, $i, $i + 1 - $i);
  $i = $i - 1;
};
  return $out;
};
  function isPalindrome($s) {
  return $s == reverseStr($s);
};
  function isPalindromeBin($n) {
  $b = toBase($n, 2);
  return isPalindrome($b);
};
  function myMin($a, $b) {
  if ($a < $b) {
  return $a;
}
  return $b;
};
  function myMax($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function reverse3($n) {
  $x = 0;
  $y = $n;
  while ($y != 0) {
  $x = $x * 3 + ($y % 3);
  $y = intval((_intdiv($y, 3)));
};
  return $x;
};
  function show($n) {
  echo rtrim('Decimal : ' . _str($n)), PHP_EOL;
  echo rtrim('Binary  : ' . toBase($n, 2)), PHP_EOL;
  echo rtrim('Ternary : ' . toBase($n, 3)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
};
  function main() {
  echo rtrim('The first 6 numbers which are palindromic in both binary and ternary are :
'), PHP_EOL;
  show(0);
  $count = 1;
  $lo = 0;
  $hi = 1;
  $pow2 = 1;
  $pow3 = 1;
  while (true) {
  $i = $lo;
  while ($i < $hi) {
  $n = ($i * 3 + 1) * $pow3 + reverse3($i);
  if (isPalindromeBin($n)) {
  show($n);
  $count = $count + 1;
  if ($count >= 6) {
  return;
};
}
  $i = $i + 1;
};
  if ($i == $pow3) {
  $pow3 = $pow3 * 3;
} else {
  $pow2 = $pow2 * 4;
}
  while (true) {
  while ($pow2 <= $pow3) {
  $pow2 = $pow2 * 4;
};
  $lo2 = intval((_intdiv((_intdiv($pow2, $pow3) - 1), 3)));
  $hi2 = intval((_intdiv((_intdiv($pow2 * 2, $pow3) - 1), 3))) + 1;
  $lo3 = intval((_intdiv($pow3, 3)));
  $hi3 = $pow3;
  if ($lo2 >= $hi3) {
  $pow3 = $pow3 * 3;
} else {
  if ($lo3 >= $hi2) {
  $pow2 = $pow2 * 4;
} else {
  $lo = myMax($lo2, $lo3);
  $hi = myMin($hi2, $hi3);
  break;
};
}
};
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
