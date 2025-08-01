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
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function maxf($a, $b) {
  if ($a > $b) {
  return $a;
}
  return $b;
};
  function isClose($a, $b) {
  $relTol = 0.000000001;
  $t = mochi_abs($a - $b);
  $u = $relTol * maxf(mochi_abs($a), mochi_abs($b));
  return $t <= $u;
};
  function sqrtApprox($x) {
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function main() {
  $root2 = sqrtApprox(2.0);
  $pairs = [[100000000000000.02, 100000000000000.02], [100.01, 100.011], [10000000000000.002 / 10000.0, 1000000000.0000001], [0.001, 0.0010000001], [0.000000000000000000000101, 0.0], [$root2 * $root2, 2.0], [(-$root2) * $root2, -2.0], [100000000000000000.0, 100000000000000000.0], [3.141592653589793, 3.141592653589793]];
  foreach ($pairs as $pair) {
  $a = $pair[0];
  $b = $pair[1];
  $s = (isClose($a, $b) ? '≈' : '≉');
  echo rtrim(_str($a) . ' ' . $s . ' ' . _str($b)), PHP_EOL;
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
