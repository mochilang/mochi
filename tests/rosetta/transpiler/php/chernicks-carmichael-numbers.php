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
  function isPrime($n) {
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
};
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
  function bigToString($a) {
  $s = '';
  $i = count($a) - 1;
  while ($i >= 0) {
  $s = $s . _str($a[$i]);
  $i = $i - 1;
};
  return $s;
};
  function pow2($k) {
  $r = 1;
  $i = 0;
  while ($i < $k) {
  $r = $r * 2;
  $i = $i + 1;
};
  return $r;
};
  function ccFactors($n, $m) {
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
};
  function ccNumbers($start, $end) {
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
};
  ccNumbers(3, 9);
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
