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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
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
  function abs_int($x) {
  if ($x < 0) {
  return -$x;
}
  return $x;
};
  function gcd_iter($a, $b) {
  $x = abs_int($a);
  $y = abs_int($b);
  while ($y != 0) {
  $t = $y;
  $y = $x % $y;
  $x = $t;
};
  return $x;
};
  function is_prime($n) {
  if ($n <= 1) {
  return false;
}
  $d = 2;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 1;
};
  return true;
};
  function sieve_er($n) {
  $nums = [];
  $i = 2;
  while ($i <= $n) {
  $nums = _append($nums, $i);
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < count($nums)) {
  $j = $idx + 1;
  while ($j < count($nums)) {
  if ($nums[$idx] != 0) {
  if (fmod($nums[$j], $nums[$idx]) == 0) {
  $nums[$j] = 0;
};
}
  $j = $j + 1;
};
  $idx = $idx + 1;
};
  $res = [];
  $k = 0;
  while ($k < count($nums)) {
  $v = $nums[$k];
  if ($v != 0) {
  $res = _append($res, $v);
}
  $k = $k + 1;
};
  return $res;
};
  function get_prime_numbers($n) {
  $ans = [];
  $num = 2;
  while ($num <= $n) {
  if (is_prime($num)) {
  $ans = _append($ans, $num);
}
  $num = $num + 1;
};
  return $ans;
};
  function prime_factorization($number) {
  if ($number == 0) {
  return [0];
}
  if ($number == 1) {
  return [1];
}
  $ans = [];
  if (is_prime($number)) {
  $ans = _append($ans, $number);
  return $ans;
}
  $quotient = $number;
  $factor = 2;
  while ($quotient != 1) {
  if (is_prime($factor) && $quotient % $factor == 0) {
  $ans = _append($ans, $factor);
  $quotient = _intdiv($quotient, $factor);
} else {
  $factor = $factor + 1;
}
};
  return $ans;
};
  function greatest_prime_factor($number) {
  $factors = prime_factorization($number);
  $m = $factors[0];
  $i = 1;
  while ($i < count($factors)) {
  if ($factors[$i] > $m) {
  $m = $factors[$i];
}
  $i = $i + 1;
};
  return $m;
};
  function smallest_prime_factor($number) {
  $factors = prime_factorization($number);
  $m = $factors[0];
  $i = 1;
  while ($i < count($factors)) {
  if ($factors[$i] < $m) {
  $m = $factors[$i];
}
  $i = $i + 1;
};
  return $m;
};
  function kg_v($number1, $number2) {
  if ($number1 < 1 || $number2 < 1) {
  $panic('numbers must be positive');
}
  $g = gcd_iter($number1, $number2);
  return (_intdiv($number1, $g)) * $number2;
};
  function is_even($number) {
  return $number % 2 == 0;
};
  function is_odd($number) {
  return $number % 2 != 0;
};
  function goldbach($number) {
  if (!is_even($number) || $number <= 2) {
  $panic('number must be even and > 2');
}
  $primes = get_prime_numbers($number);
  $i = 0;
  while ($i < count($primes)) {
  $j = $i + 1;
  while ($j < count($primes)) {
  if ($primes[$i] + $primes[$j] == $number) {
  return [$primes[$i], $primes[$j]];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return [];
};
  function get_prime($n) {
  if ($n < 0) {
  $panic('n must be non-negative');
}
  $index = 0;
  $ans = 2;
  while ($index < $n) {
  $index = $index + 1;
  $ans = $ans + 1;
  while (!is_prime($ans)) {
  $ans = $ans + 1;
};
};
  return $ans;
};
  function get_primes_between($p1, $p2) {
  $bad1 = !is_prime($p1);
  $bad2 = !is_prime($p2);
  if ($bad1 || $bad2 || $p1 >= $p2) {
  $panic('arguments must be prime and p1 < p2');
}
  $num = $p1 + 1;
  while ($num < $p2) {
  if (is_prime($num)) {
  break;
}
  $num = $num + 1;
};
  $ans = [];
  while ($num < $p2) {
  $ans = _append($ans, $num);
  $num = $num + 1;
  while ($num < $p2) {
  if (is_prime($num)) {
  break;
}
  $num = $num + 1;
};
};
  return $ans;
};
  function get_divisors($n) {
  if ($n < 1) {
  $panic('n must be >= 1');
}
  $ans = [];
  $d = 1;
  while ($d <= $n) {
  if ($n % $d == 0) {
  $ans = _append($ans, $d);
}
  $d = $d + 1;
};
  return $ans;
};
  function is_perfect_number($number) {
  if ($number <= 1) {
  $panic('number must be > 1');
}
  $divisors = get_divisors($number);
  $sum = 0;
  $i = 0;
  while ($i < count($divisors) - 1) {
  $sum = $sum + $divisors[$i];
  $i = $i + 1;
};
  return $sum == $number;
};
  function simplify_fraction($numerator, $denominator) {
  if ($denominator == 0) {
  $panic('denominator cannot be zero');
}
  $g = gcd_iter(abs_int($numerator), abs_int($denominator));
  return [_intdiv($numerator, $g), _intdiv($denominator, $g)];
};
  function factorial($n) {
  if ($n < 0) {
  $panic('n must be >= 0');
}
  $ans = 1;
  $i = 1;
  while ($i <= $n) {
  $ans = $ans * $i;
  $i = $i + 1;
};
  return $ans;
};
  function fib($n) {
  if ($n < 0) {
  $panic('n must be >= 0');
}
  if ($n <= 1) {
  return 1;
}
  $tmp = 0;
  $fib1 = 1;
  $ans = 1;
  $i = 0;
  while ($i < $n - 1) {
  $tmp = $ans;
  $ans = $ans + $fib1;
  $fib1 = $tmp;
  $i = $i + 1;
};
  return $ans;
};
  echo rtrim(_str(is_prime(97))), PHP_EOL;
  echo rtrim(_str(sieve_er(20))), PHP_EOL;
  echo rtrim(_str(get_prime_numbers(20))), PHP_EOL;
  echo rtrim(_str(prime_factorization(287))), PHP_EOL;
  echo rtrim(_str(greatest_prime_factor(287))), PHP_EOL;
  echo rtrim(_str(smallest_prime_factor(287))), PHP_EOL;
  echo rtrim(_str(kg_v(8, 10))), PHP_EOL;
  echo rtrim(_str(goldbach(28))), PHP_EOL;
  echo rtrim(_str(get_prime(8))), PHP_EOL;
  echo rtrim(_str(get_primes_between(3, 20))), PHP_EOL;
  echo rtrim(_str(get_divisors(28))), PHP_EOL;
  echo rtrim(_str(is_perfect_number(28))), PHP_EOL;
  echo rtrim(_str(simplify_fraction(10, 20))), PHP_EOL;
  echo rtrim(_str(factorial(5))), PHP_EOL;
  echo rtrim(_str(fib(10))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
