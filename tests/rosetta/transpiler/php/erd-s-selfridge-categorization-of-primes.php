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
  function generatePrimes($n) {
  global $prevCats;
  $primes = [2];
  $cand = 3;
  while (count($primes) < $n) {
  $isP = true;
  $i = 0;
  while ($i < count($primes)) {
  $p = $primes[$i];
  if ($p * $p > $cand) {
  break;
}
  if ($cand % $p == 0) {
  $isP = false;
  break;
}
  $i = $i + 1;
};
  if ($isP) {
  $primes = array_merge($primes, [$cand]);
}
  $cand = $cand + 2;
};
  return $primes;
};
  function primeFactors($n, $primes) {
  global $prevCats;
  $factors = [];
  $num = $n;
  $i = 0;
  while ($i < count($primes) && $primes[$i] * $primes[$i] <= $num) {
  $p = $primes[$i];
  while ($num % $p == 0) {
  $factors = array_merge($factors, [$p]);
  $num = _intdiv($num, $p);
};
  $i = $i + 1;
};
  if ($num > 1) {
  $factors = array_merge($factors, [$num]);
}
  return $factors;
};
  $prevCats = [];
  function cat($p, $primes) {
  global $prevCats;
  if (array_key_exists($p, $prevCats)) {
  return $prevCats[$p];
}
  $pf = primeFactors($p + 1, $primes);
  $all23 = true;
  foreach ($pf as $f) {
  if ($f != 2 && $f != 3) {
  $all23 = false;
  break;
}
};
  if ($all23) {
  $prevCats[$p] = 1;
  return 1;
}
  if ($p > 2) {
  $unique = [];
  $last = -1;
  foreach ($pf as $f) {
  if ($f != $last) {
  $unique = array_merge($unique, [$f]);
  $last = $f;
}
};
  $pf = $unique;
}
  $c = 2;
  while ($c <= 11) {
  $ok = true;
  foreach ($pf as $f) {
  if (cat($f, $primes) >= $c) {
  $ok = false;
  break;
}
};
  if ($ok) {
  $prevCats[$p] = $c;
  return $c;
}
  $c = $c + 1;
};
  $prevCats[$p] = 12;
  return 12;
};
  function padLeft($n, $width) {
  global $prevCats;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function main() {
  global $prevCats;
  $primes = generatePrimes(1000);
  $es = [];
  for ($_ = 0; $_ < 12; $_++) {
  $es = array_merge($es, [[]]);
};
  echo rtrim('First 200 primes:
'), PHP_EOL;
  $idx = 0;
  while ($idx < 200) {
  $p = $primes[$idx];
  $c = cat($p, $primes);
  $es[$c - 1] = array_merge($es[$c - 1], [$p]);
  $idx = $idx + 1;
};
  $c = 1;
  while ($c <= 6) {
  if (count($es[$c - 1]) > 0) {
  echo rtrim('Category ' . _str($c) . ':'), PHP_EOL;
  echo rtrim(_str($es[$c - 1])), PHP_EOL;
  echo rtrim(''), PHP_EOL;
}
  $c = $c + 1;
};
  echo rtrim('First thousand primes:
'), PHP_EOL;
  while ($idx < 1000) {
  $p = $primes[$idx];
  $cv = cat($p, $primes);
  $es[$cv - 1] = array_merge($es[$cv - 1], [$p]);
  $idx = $idx + 1;
};
  $c = 1;
  while ($c <= 12) {
  $e = $es[$c - 1];
  if (count($e) > 0) {
  $line = 'Category ' . padLeft($c, 2) . ': First = ' . padLeft($e[0], 7) . '  Last = ' . padLeft($e[count($e) - 1], 8) . '  Count = ' . padLeft(count($e), 6);
  echo rtrim($line), PHP_EOL;
}
  $c = $c + 1;
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
