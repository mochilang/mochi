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
  function powf($base, $exp) {
  $result = 1.0;
  $i = 0;
  while ($i < $exp) {
  $result = $result * $base;
  $i = $i + 1;
};
  return $result;
};
  function nthRoot($x, $n) {
  $low = 0.0;
  $high = $x;
  $i = 0;
  while ($i < 60) {
  $mid = ($low + $high) / 2.0;
  if (powf($mid, $n) > $x) {
  $high = $mid;
} else {
  $low = $mid;
}
  $i = $i + 1;
};
  return $low;
};
  function main() {
  $sum = 0.0;
  $sumRecip = 0.0;
  $prod = 1.0;
  $n = 1;
  while ($n <= 10) {
  $f = floatval($n);
  $sum = $sum + $f;
  $sumRecip = $sumRecip + 1.0 / $f;
  $prod = $prod * $f;
  $n = $n + 1;
};
  $count = 10.0;
  $a = $sum / $count;
  $g = nthRoot($prod, 10);
  $h = $count / $sumRecip;
  echo rtrim('A: ' . _str($a) . ' G: ' . _str($g) . ' H: ' . _str($h)), PHP_EOL;
  echo rtrim('A >= G >= H: ' . _str($a >= $g && $g >= $h)), PHP_EOL;
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
