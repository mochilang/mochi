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
  function sieve($limit) {
  $spf = [];
  $i = 0;
  while ($i <= $limit) {
  $spf = array_merge($spf, [0]);
  $i = $i + 1;
};
  $i = 2;
  while ($i <= $limit) {
  if ($spf[$i] == 0) {
  $spf[$i] = $i;
  if ($i * $i <= $limit) {
  $j = $i * $i;
  while ($j <= $limit) {
  if ($spf[$j] == 0) {
  $spf[$j] = $i;
}
  $j = $j + $i;
};
};
}
  $i = $i + 1;
};
  return $spf;
};
  function primesFrom($spf, $limit) {
  $primes = [];
  $i = 3;
  while ($i <= $limit) {
  if ($spf[$i] == $i) {
  $primes = array_merge($primes, [$i]);
}
  $i = $i + 1;
};
  return $primes;
};
  function pad3($n) {
  $s = _str($n);
  while (strlen($s) < 3) {
  $s = ' ' . $s;
};
  return $s;
};
  function commatize($n) {
  $s = _str($n);
  $out = '';
  $i = strlen($s) - 1;
  $c = 0;
  while ($i >= 0) {
  $out = substr($s, $i, $i + 1 - $i) . $out;
  $c = $c + 1;
  if ($c % 3 == 0 && $i > 0) {
  $out = ',' . $out;
}
  $i = $i - 1;
};
  return $out;
};
  function primeCount($primes, $last, $spf) {
  $lo = 0;
  $hi = count($primes);
  while ($lo < $hi) {
  $mid = intval((_intdiv(($lo + $hi), 2)));
  if ($primes[$mid] < $last) {
  $lo = $mid + 1;
} else {
  $hi = $mid;
}
};
  $count = $lo + 1;
  if ($spf[$last] != $last) {
  $count = $count - 1;
}
  return $count;
};
  function arithmeticNumbers($limit, $spf) {
  $arr = [1];
  $n = 3;
  while (count($arr) < $limit) {
  if ($spf[$n] == $n) {
  $arr = array_merge($arr, [$n]);
} else {
  $x = $n;
  $sigma = 1;
  $tau = 1;
  while ($x > 1) {
  $p = $spf[$x];
  if ($p == 0) {
  $p = $x;
}
  $cnt = 0;
  $power = $p;
  $sum = 1;
  while ($x % $p == 0) {
  $x = _intdiv($x, $p);
  $cnt = $cnt + 1;
  $sum = $sum + $power;
  $power = $power * $p;
};
  $sigma = $sigma * $sum;
  $tau = $tau * ($cnt + 1);
};
  if ($sigma % $tau == 0) {
  $arr = array_merge($arr, [$n]);
};
}
  $n = $n + 1;
};
  return $arr;
};
  function main() {
  $limit = 1228663;
  $spf = sieve($limit);
  $primes = primesFrom($spf, $limit);
  $arr = arithmeticNumbers(1000000, $spf);
  echo rtrim('The first 100 arithmetic numbers are:'), PHP_EOL;
  $i = 0;
  while ($i < 100) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . pad3($arr[$i + $j]);
  if ($j < 9) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 10;
};
  foreach ([1000, 10000, 100000, 1000000] as $x) {
  $last = $arr[$x - 1];
  $lastc = commatize($last);
  echo rtrim('
The ' . commatize($x) . 'th arithmetic number is: ' . $lastc), PHP_EOL;
  $pc = primeCount($primes, $last, $spf);
  $comp = $x - $pc - 1;
  echo rtrim('The count of such numbers <= ' . $lastc . ' which are composite is ' . commatize($comp) . '.'), PHP_EOL;
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
