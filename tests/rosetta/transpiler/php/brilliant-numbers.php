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
$__start_mem = memory_get_usage();
$__start = _now();
  function primesUpTo($n) {
  global $primes;
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
  function sortInts($xs) {
  global $primes;
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
  function commatize($n) {
  global $primes;
  $s = _str($n);
  $i = strlen($s) - 3;
  while ($i >= 1) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  return $s;
};
  $primes = primesUpTo(3200000);
  function getBrilliant($digits, $limit, $countOnly) {
  global $primes;
  $brilliant = [];
  $count = 0;
  $pow = 1;
  $next = 999999999999999;
  $k = 1;
  while ($k <= $digits) {
  $s = [];
  foreach ($primes as $p) {
  if ($p >= $pow * 10) {
  break;
}
  if ($p > $pow) {
  $s = array_merge($s, [$p]);
}
};
  $i = 0;
  while ($i < count($s)) {
  $j = $i;
  while ($j < count($s)) {
  $prod = $s[$i] * $s[$j];
  if ($prod < $limit) {
  if ($countOnly) {
  $count = $count + 1;
} else {
  $brilliant = array_merge($brilliant, [$prod]);
};
} else {
  if ($prod < $next) {
  $next = $prod;
};
  break;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $pow = $pow * 10;
  $k = $k + 1;
};
  if ($countOnly) {
  return ['bc' => $count, 'next' => $next];
}
  return ['bc' => $brilliant, 'next' => $next];
};
  function main() {
  global $primes;
  echo rtrim('First 100 brilliant numbers:'), PHP_EOL;
  $r = getBrilliant(2, 10000, false);
  $br = sortInts($r['bc']);
  $br = array_slice($br, 0, 100 - 0);
  $i = 0;
  while ($i < count($br)) {
  echo rtrim(str_pad(_str($br[$i]), 4, ' ', STR_PAD_LEFT) . ' ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  if (($i + 1) % 10 == 0) {
  echo rtrim('') . " " . rtrim((true ? 'true' : 'false')), PHP_EOL;
}
  $i = $i + 1;
};
  echo rtrim('') . " " . rtrim((true ? 'true' : 'false')), PHP_EOL;
  $k = 1;
  while ($k <= 13) {
  $limit = $pow(10, $k);
  $r2 = getBrilliant($k, $limit, true);
  $total = $r2['bc'];
  $next = $r2['next'];
  $climit = commatize($limit);
  $ctotal = commatize($total + 1);
  $cnext = commatize($next);
  echo rtrim('First >= ' . str_pad($climit, 18, ' ', STR_PAD_LEFT) . ' is ' . str_pad($ctotal, 14, ' ', STR_PAD_LEFT) . ' in the series: ' . str_pad($cnext, 18, ' ', STR_PAD_LEFT)), PHP_EOL;
  $k = $k + 1;
};
};
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
