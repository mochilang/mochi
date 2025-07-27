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
  function sqrtApprox($x) {
  if ($x <= 0.0) {
  return 0.0;
}
  $g = $x;
  $i = 0;
  while ($i < 20) {
  $g = ($g + $x / $g) / 2.0;
  $i = $i + 1;
};
  return $g;
};
  function mochi_abs($x) {
  if ($x < 0.0) {
  return -$x;
}
  return $x;
};
  function main() {
  $oldPhi = 1.0;
  $phi = 0.0;
  $iters = 0;
  $limit = 0.00001;
  while (true) {
  $phi = 1.0 + 1.0 / $oldPhi;
  $iters = $iters + 1;
  if (mochi_abs($phi - $oldPhi) <= $limit) {
  break;
}
  $oldPhi = $phi;
};
  $actual = (1.0 + sqrtApprox(5.0)) / 2.0;
  echo rtrim('Final value of phi : ' . _str($phi)), PHP_EOL;
  echo rtrim('Number of iterations : ' . _str($iters)), PHP_EOL;
  echo rtrim('Error (approx) : ' . _str($phi - $actual)), PHP_EOL;
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
