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
  function randOrder($seed, $n) {
  $next = ($seed * 1664525 + 1013904223) % 2147483647;
  return [$next, $next % $n];
};
  function randChaos($seed, $n) {
  $next = ($seed * 1103515245 + 12345) % 2147483647;
  return [$next, $next % $n];
};
  function main() {
  $nBuckets = 10;
  $initialSum = 1000;
  $buckets = [];
  for ($i = 0; $i < $nBuckets; $i++) {
  $buckets = array_merge($buckets, [0]);
};
  $i = $nBuckets;
  $dist = $initialSum;
  while ($i > 0) {
  $v = _intdiv($dist, $i);
  $i = $i - 1;
  $buckets[$i] = $v;
  $dist = $dist - $v;
};
  $tc0 = 0;
  $tc1 = 0;
  $total = 0;
  $nTicks = 0;
  $seedOrder = 1;
  $seedChaos = 2;
  echo rtrim('sum  ---updates---    mean  buckets'), PHP_EOL;
  $t = 0;
  while ($t < 5) {
  $r = randOrder($seedOrder, $nBuckets);
  $seedOrder = $r[0];
  $b1 = $r[1];
  $b2 = ($b1 + 1) % $nBuckets;
  $v1 = $buckets[$b1];
  $v2 = $buckets[$b2];
  if ($v1 > $v2) {
  $a = intval((_intdiv(($v1 - $v2), 2)));
  if ($a > $buckets[$b1]) {
  $a = $buckets[$b1];
};
  $buckets[$b1] = $buckets[$b1] - $a;
  $buckets[$b2] = $buckets[$b2] + $a;
} else {
  $a = intval((_intdiv(($v2 - $v1), 2)));
  if ($a > $buckets[$b2]) {
  $a = $buckets[$b2];
};
  $buckets[$b2] = $buckets[$b2] - $a;
  $buckets[$b1] = $buckets[$b1] + $a;
}
  $tc0 = $tc0 + 1;
  $r = randChaos($seedChaos, $nBuckets);
  $seedChaos = $r[0];
  $b1 = $r[1];
  $b2 = ($b1 + 1) % $nBuckets;
  $r = randChaos($seedChaos, $buckets[$b1] + 1);
  $seedChaos = $r[0];
  $amt = $r[1];
  if ($amt > $buckets[$b1]) {
  $amt = $buckets[$b1];
}
  $buckets[$b1] = $buckets[$b1] - $amt;
  $buckets[$b2] = $buckets[$b2] + $amt;
  $tc1 = $tc1 + 1;
  $sum = 0;
  $idx = 0;
  while ($idx < $nBuckets) {
  $sum = $sum + $buckets[$idx];
  $idx = $idx + 1;
};
  $total = $total + $tc0 + $tc1;
  $nTicks = $nTicks + 1;
  echo rtrim(_str($sum) . ' ' . _str($tc0) . ' ' . _str($tc1) . ' ' . _str(_intdiv($total, $nTicks)) . '  ' . _str($buckets)), PHP_EOL;
  $tc0 = 0;
  $tc1 = 0;
  $t = $t + 1;
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
