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
  function pfacSum($i) {
  $sum = 0;
  $p = 1;
  while ($p <= _intdiv($i, 2)) {
  if ($i % $p == 0) {
  $sum = $sum + $p;
}
  $p = $p + 1;
};
  return $sum;
};
  function main() {
  $d = 0;
  $a = 0;
  $pnum = 0;
  $i = 1;
  while ($i <= 20000) {
  $j = pfacSum($i);
  if ($j < $i) {
  $d = $d + 1;
}
  if ($j == $i) {
  $pnum = $pnum + 1;
}
  if ($j > $i) {
  $a = $a + 1;
}
  $i = $i + 1;
};
  echo rtrim('There are ' . _str($d) . ' deficient numbers between 1 and 20000'), PHP_EOL;
  echo rtrim('There are ' . _str($a) . ' abundant numbers  between 1 and 20000'), PHP_EOL;
  echo rtrim('There are ' . _str($pnum) . ' perfect numbers between 1 and 20000'), PHP_EOL;
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
