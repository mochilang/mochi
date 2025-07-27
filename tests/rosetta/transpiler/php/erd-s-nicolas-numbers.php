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
  $maxNumber = 100000000;
  $dsum = [];
  $dcount = [];
  $i = 0;
  while ($i <= $maxNumber) {
  $dsum = array_merge($dsum, [1]);
  $dcount = array_merge($dcount, [1]);
  $i = $i + 1;
}
  function pad8($n) {
  global $maxNumber, $dsum, $dcount, $i, $j;
  $s = _str($n);
  while (strlen($s) < 8) {
  $s = ' ' . $s;
};
  return $s;
};
  $i = 2;
  while ($i <= $maxNumber) {
  $j = $i + $i;
  while ($j <= $maxNumber) {
  if ($dsum[$j] == $j) {
  echo rtrim(pad8($j) . ' equals the sum of its first ' . _str($dcount[$j]) . ' divisors'), PHP_EOL;
}
  $dsum[$j] = $dsum[$j] + $i;
  $dcount[$j] = $dcount[$j] + 1;
  $j = $j + $i;
};
  $i = $i + 1;
}
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
