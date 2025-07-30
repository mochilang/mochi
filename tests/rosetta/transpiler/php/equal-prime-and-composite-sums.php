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
  function commatize($n) {
  $s = _str($n);
  $i = strlen($s) - 3;
  while ($i >= 1) {
  $s = substr($s, 0, $i - 0) . ',' . substr($s, $i, strlen($s) - $i);
  $i = $i - 3;
};
  return $s;
};
  function primeSieve($n) {
  $sieve = [];
  $i = 0;
  while ($i <= $n) {
  $sieve = array_merge($sieve, [false]);
  $i = $i + 1;
};
  $sieve[0] = true;
  $sieve[1] = true;
  $p = 2;
  while ($p * $p <= $n) {
  if (!$sieve[$p]) {
  $m = $p * $p;
  while ($m <= $n) {
  $sieve[$m] = true;
  $m = $m + $p;
};
}
  $p = $p + 1;
};
  return $sieve;
};
  function search($xs, $target) {
  $low = 0;
  $high = count($xs);
  while ($low < $high) {
  $mid = _intdiv(($low + $high), 2);
  if ($xs[$mid] < $target) {
  $low = $mid + 1;
} else {
  $high = $mid;
}
};
  return $low;
};
  function main() {
  $limit = 45000;
  $compMap = primeSieve($limit);
  $compSums = [];
  $primeSums = [];
  $csum = 0;
  $psum = 0;
  $i = 2;
  while ($i <= $limit) {
  if ($compMap[$i]) {
  $csum = $csum + $i;
  $compSums = array_merge($compSums, [$csum]);
} else {
  $psum = $psum + $i;
  $primeSums = array_merge($primeSums, [$psum]);
}
  $i = $i + 1;
};
  echo rtrim('Sum        | Prime Index | Composite Index'), PHP_EOL;
  echo rtrim('------------------------------------------'), PHP_EOL;
  $idx = 0;
  while ($idx < count($primeSums)) {
  $s = $primeSums[$idx];
  $j = search($compSums, $s);
  if ($j < count($compSums) && $compSums[$j] == $s) {
  $sumStr = str_pad(commatize($s), 10, ' ', STR_PAD_LEFT);
  $piStr = str_pad(commatize($idx + 1), 11, ' ', STR_PAD_LEFT);
  $ciStr = str_pad(commatize($j + 1), 15, ' ', STR_PAD_LEFT);
  echo rtrim($sumStr . ' | ' . $piStr . ' | ' . $ciStr), PHP_EOL;
}
  $idx = $idx + 1;
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
