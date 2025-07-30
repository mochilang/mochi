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
  function expI($b, $p) {
  $r = 1;
  $i = 0;
  while ($i < $p) {
  $r = $r * $b;
  $i = $i + 1;
};
  return $r;
};
  function expF($b, $p) {
  $r = 1.0;
  $pow = $b;
  $n = $p;
  $neg = false;
  if ($p < 0) {
  $n = -$p;
  $neg = true;
}
  while ($n > 0) {
  if ($n % 2 == 1) {
  $r = $r * $pow;
}
  $pow = $pow * $pow;
  $n = _intdiv($n, 2);
};
  if ($neg) {
  $r = 1.0 / $r;
}
  return $r;
};
  function printExpF($b, $p) {
  if ($b == 0.0 && $p < 0) {
  echo rtrim(_str($b) . '^' . _str($p) . ': +Inf'), PHP_EOL;
} else {
  echo rtrim(_str($b) . '^' . _str($p) . ': ' . _str(expF($b, $p))), PHP_EOL;
}
};
  function main() {
  echo rtrim('expI tests'), PHP_EOL;
  foreach ([[2, 10], [2, -10], [-2, 10], [-2, 11], [11, 0]] as $pair) {
  if ($pair[1] < 0) {
  echo rtrim(_str($pair[0]) . '^' . _str($pair[1]) . ': negative power not allowed'), PHP_EOL;
} else {
  echo rtrim(_str($pair[0]) . '^' . _str($pair[1]) . ': ' . _str(expI($pair[0], $pair[1]))), PHP_EOL;
}
};
  echo rtrim('overflow undetected'), PHP_EOL;
  echo rtrim('10^10: ' . _str(expI(10, 10))), PHP_EOL;
  echo rtrim('
expF tests:'), PHP_EOL;
  foreach ([[2.0, 10], [2.0, -10], [-2.0, 10], [-2.0, 11], [11.0, 0]] as $pair) {
  printExpF($pair[0], $pair[1]);
};
  echo rtrim('disallowed in expI, allowed here'), PHP_EOL;
  printExpF(0.0, -1);
  echo rtrim('other interesting cases for 32 bit float type'), PHP_EOL;
  printExpF(10.0, 39);
  printExpF(10.0, -39);
  printExpF(-10.0, 39);
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
