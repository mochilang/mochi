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
  $seed = 1;
  function set_seed($s) {
  global $seed;
  $seed = $s;
};
  function randint($a, $b) {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return ($seed % ($b - $a + 1)) + $a;
};
  function jacobi_symbol($random_a, $number) {
  global $seed;
  if ($random_a == 0 || $random_a == 1) {
  return $random_a;
}
  $random_a = $random_a % $number;
  $t = 1;
  while ($random_a != 0) {
  while ($random_a % 2 == 0) {
  $random_a = _intdiv($random_a, 2);
  $r = $number % 8;
  if ($r == 3 || $r == 5) {
  $t = -$t;
}
};
  $temp = $random_a;
  $random_a = $number;
  $number = $temp;
  if ($random_a % 4 == 3 && $number % 4 == 3) {
  $t = -$t;
}
  $random_a = $random_a % $number;
};
  if ($number == 1) {
  return $t;
}
  return 0;
};
  function pow_mod($base, $exp, $mod) {
  global $seed;
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
};
  function solovay_strassen($number, $iterations) {
  global $seed;
  if ($number <= 1) {
  return false;
}
  if ($number <= 3) {
  return true;
}
  $i = 0;
  while ($i < $iterations) {
  $a = randint(2, $number - 2);
  $x = jacobi_symbol($a, $number);
  $y = pow_mod($a, _intdiv(($number - 1), 2), $number);
  $mod_x = $x % $number;
  if ($mod_x < 0) {
  $mod_x = $mod_x + $number;
}
  if ($x == 0 || $y != $mod_x) {
  return false;
}
  $i = $i + 1;
};
  return true;
};
  function main() {
  global $seed;
  set_seed(10);
  echo rtrim(_str(solovay_strassen(13, 5))), PHP_EOL;
  echo rtrim(_str(solovay_strassen(9, 10))), PHP_EOL;
  echo rtrim(_str(solovay_strassen(17, 15))), PHP_EOL;
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
