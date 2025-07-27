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
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $zero = 0;
  $one = 1;
  $two = 2;
  $three = 3;
  $four = 4;
  $five = 5;
  $six = 6;
  $ten = 10;
  $k100 = 100000;
  function gcd($a, $b) {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  $x = $a;
  $y = $b;
  while ($y != $zero) {
  $t = _imod($x, $y);
  $x = $y;
  $y = $t;
};
  return $x;
};
  function absBig($x) {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  if ($x < $zero) {
  return _isub($zero, $x);
}
  return $x;
};
  function pollardRho($n, $c) {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  $g = null;
$g = function($x, $y) use (&$g, $n, $c, &$zero, &$one, &$two, &$three, &$four, &$five, &$six, &$ten, &$k100) {
  $x2 = _imul($x, $x);
  $x2 = _iadd($x2, $c);
  return _imod($x2, $y);
};
  $x = 2;
  $y = 2;
  $z = 1;
  $d = 0;
  $count = 0;
  while (true) {
  $x = $g($x, $n);
  $y = $g($g($y, $n), $n);
  $d = absBig(_isub($x, $y));
  $d = _imod($d, $n);
  $z = _imul($z, $d);
  $count = $count + 1;
  if ($count == 100) {
  $d = gcd($z, $n);
  if ($d != $one) {
  break;
};
  $z = $one;
  $count = 0;
}
};
  if ($d == $n) {
  return $zero;
}
  return $d;
};
  function smallestPrimeFactorWheel($n, $max) {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  if (_imod($n, $two) == $zero) {
  return $two;
}
  if (_imod($n, $three) == $zero) {
  return $three;
}
  if (_imod($n, $five) == $zero) {
  return $five;
}
  $k = 7;
  $inc = [$four, $two, $four, $two, $four, $six, $two, $six];
  $i = 0;
  while (_imul($k, $k) <= $n) {
  if (_imod($n, $k) == $zero) {
  return $k;
}
  $k = _iadd($k, $inc[$i]);
  if ($k > $max) {
  break;
}
  $i = ($i + 1) % 8;
};
  return $zero;
};
  function smallestPrimeFactor($n) {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  $s = smallestPrimeFactorWheel($n, $k100);
  if ($s != $zero) {
  return $s;
}
  $c = 1;
  while (true) {
  $d = pollardRho($n, $c);
  if ($d == $zero) {
  if ($c == $ten) {
  return $n;
};
  $c = _iadd($c, $one);
} else {
  $factor = smallestPrimeFactorWheel($d, $d);
  $s2 = smallestPrimeFactorWheel(_idiv($n, $d), $factor);
  if ($s2 != $zero) {
  if ($s2 < $factor) {
  return $s2;
} else {
  return $factor;
};
};
  return $factor;
}
};
};
  function main() {
  global $zero, $one, $two, $three, $four, $five, $six, $ten, $k100;
  $k = 19;
  echo rtrim('First ' . _str($k) . ' terms of the Euclid–Mullin sequence:'), PHP_EOL;
  echo rtrim(json_encode(2, 1344)), PHP_EOL;
  $prod = 2;
  $count = 1;
  while ($count < $k) {
  $z = _iadd($prod, $one);
  $t = smallestPrimeFactor($z);
  echo rtrim(json_encode($t, 1344)), PHP_EOL;
  $prod = _imul($prod, $t);
  $count = $count + 1;
};
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
