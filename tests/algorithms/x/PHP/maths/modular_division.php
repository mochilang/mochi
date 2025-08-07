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
$__start_mem = memory_get_usage();
$__start = _now();
  function mod($a, $n) {
  $r = $a % $n;
  if ($r < 0) {
  return $r + $n;
}
  return $r;
};
  function greatest_common_divisor($a, $b) {
  $x = ($a < 0 ? -$a : $a);
  $y = ($b < 0 ? -$b : $b);
  while ($y != 0) {
  $t = $x % $y;
  $x = $y;
  $y = $t;
};
  return $x;
};
  function extended_gcd($a, $b) {
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
};
  function extended_euclid($a, $b) {
  if ($b == 0) {
  return [1, 0];
}
  $res = extended_euclid($b, $a % $b);
  $x = $res[1];
  $y = $res[0] - (_intdiv($a, $b)) * $res[1];
  return [$x, $y];
};
  function invert_modulo($a, $n) {
  $res = extended_euclid($a, $n);
  $inv = $res[0];
  return mod($inv, $n);
};
  function modular_division($a, $b, $n) {
  if ($n <= 1) {
  $panic('n must be > 1');
}
  if ($a <= 0) {
  $panic('a must be > 0');
}
  if (greatest_common_divisor($a, $n) != 1) {
  $panic('gcd(a,n) != 1');
}
  $eg = extended_gcd($n, $a);
  $s = $eg[2];
  return mod($b * $s, $n);
};
  function modular_division2($a, $b, $n) {
  $s = invert_modulo($a, $n);
  return mod($b * $s, $n);
};
  function tests() {
  if (modular_division(4, 8, 5) != 2) {
  $panic('md1');
}
  if (modular_division(3, 8, 5) != 1) {
  $panic('md2');
}
  if (modular_division(4, 11, 5) != 4) {
  $panic('md3');
}
  if (modular_division2(4, 8, 5) != 2) {
  $panic('md21');
}
  if (modular_division2(3, 8, 5) != 1) {
  $panic('md22');
}
  if (modular_division2(4, 11, 5) != 4) {
  $panic('md23');
}
  if (invert_modulo(2, 5) != 3) {
  $panic('inv');
}
  $eg = extended_gcd(10, 6);
  if ($eg[0] != 2 || $eg[1] != (-1) || $eg[2] != 2) {
  $panic('eg');
}
  $eu = extended_euclid(10, 6);
  if ($eu[0] != (-1) || $eu[1] != 2) {
  $panic('eu');
}
  if (greatest_common_divisor(121, 11) != 11) {
  $panic('gcd');
}
};
  function main() {
  tests();
  echo rtrim(_str(modular_division(4, 8, 5))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
