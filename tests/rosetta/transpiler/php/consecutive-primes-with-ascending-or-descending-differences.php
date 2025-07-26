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
  global $LIMIT, $primes;
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
  $LIMIT = 999999;
  $primes = primesUpTo($LIMIT);
  function longestSeq($dir) {
  global $LIMIT, $primes;
  $pd = 0;
  $longSeqs = [[2]];
  $currSeq = [2];
  $i = 1;
  while ($i < count($primes)) {
  $d = $primes[$i] - $primes[$i - 1];
  if (($dir == 'ascending' && $d <= $pd) || ($dir == 'descending' && $d >= $pd)) {
  if (count($currSeq) > count($longSeqs[0])) {
  $longSeqs = [$currSeq];
} else {
  if (count($currSeq) == count($longSeqs[0])) {
  $longSeqs = array_merge($longSeqs, [$currSeq]);
};
};
  $currSeq = [$primes[$i - 1], $primes[$i]];
} else {
  $currSeq = array_merge($currSeq, [$primes[$i]]);
}
  $pd = $d;
  $i = $i + 1;
};
  if (count($currSeq) > count($longSeqs[0])) {
  $longSeqs = [$currSeq];
} else {
  if (count($currSeq) == count($longSeqs[0])) {
  $longSeqs = array_merge($longSeqs, [$currSeq]);
};
}
  echo rtrim('Longest run(s) of primes with ' . $dir . ' differences is ' . _str(count($longSeqs[0])) . ' :'), PHP_EOL;
  foreach ($longSeqs as $ls) {
  $diffs = [];
  $j = 1;
  while ($j < count($ls)) {
  $diffs = array_merge($diffs, [$ls[$j] - $ls[$j - 1]]);
  $j = $j + 1;
};
  $k = 0;
  while ($k < count($ls) - 1) {
  echo rtrim(_str($ls[$k]) . ' (' . _str($diffs[$k]) . ') ') . " " . rtrim((false ? 'true' : 'false')), PHP_EOL;
  $k = $k + 1;
};
  echo rtrim(_str($ls[count($ls) - 1])), PHP_EOL;
};
  echo rtrim(''), PHP_EOL;
};
  function main() {
  global $LIMIT, $primes;
  echo rtrim('For primes < 1 million:\n'), PHP_EOL;
  foreach (['ascending', 'descending'] as $dir) {
  longestSeq($dir);
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
