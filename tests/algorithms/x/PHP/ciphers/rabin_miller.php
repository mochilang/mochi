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
  function int_pow($base, $exp) {
  global $p;
  $result = 1;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function pow_mod($base, $exp, $mod) {
  global $p;
  $result = 1;
  $b = $base % $mod;
  $e = $exp;
  while ($e > 0) {
  if ($e % 2 == 1) {
  $result = ($result * $b) % $mod;
}
  $e = _intdiv($e, 2);
  $b = ($b * $b) % $mod;
};
  return $result;
};
  function rand_range($low, $high) {
  global $p;
  return (fmod(_now(), ($high - $low))) + $low;
};
  function rabin_miller($num) {
  global $p;
  $s = $num - 1;
  $t = 0;
  while ($s % 2 == 0) {
  $s = _intdiv($s, 2);
  $t = $t + 1;
};
  $k = 0;
  while ($k < 5) {
  $a = rand_range(2, $num - 1);
  $v = pow_mod($a, $s, $num);
  if ($v != 1) {
  $i = 0;
  while ($v != ($num - 1)) {
  if ($i == $t - 1) {
  return false;
}
  $i = $i + 1;
  $v = ($v * $v) % $num;
};
}
  $k = $k + 1;
};
  return true;
};
  function is_prime_low_num($num) {
  if ($num < 2) {
  return false;
}
  $low_primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997];
  if (in_array($num, $low_primes)) {
  return true;
}
  $i = 0;
  while ($i < count($low_primes)) {
  $p = $low_primes[$i];
  if ($num % $p == 0) {
  return false;
}
  $i = $i + 1;
};
  return rabin_miller($num);
};
  function generate_large_prime($keysize) {
  global $p;
  $start = int_pow(2, $keysize - 1);
  $end = int_pow(2, $keysize);
  while (true) {
  $num = rand_range($start, $end);
  if (is_prime_low_num($num)) {
  return $num;
}
};
};
  $p = generate_large_prime(16);
  echo rtrim('Prime number: ' . _str($p)), PHP_EOL;
  echo rtrim('is_prime_low_num: ' . _str(is_prime_low_num($p))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
