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
  function show($xs) {
  $s = '';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ' ';
}
  $i = $i + 1;
};
  return $s;
};
  function gen($init, $n) {
  $b = $init;
  $res = [];
  $sum = 0;
  foreach ($b as $x) {
  $res = array_merge($res, [$x]);
  $sum = $sum + $x;
};
  while (count($res) < $n) {
  $next = $sum;
  $res = array_merge($res, [$next]);
  $sum = $sum + $next - $b[0];
  $b = array_merge(array_slice($b, 1, count($b) - 1), [$next]);
};
  return $res;
};
  function main() {
  $n = 10;
  echo rtrim(' Fibonacci: ' . show(gen([1, 1], $n))), PHP_EOL;
  echo rtrim('Tribonacci: ' . show(gen([1, 1, 2], $n))), PHP_EOL;
  echo rtrim('Tetranacci: ' . show(gen([1, 1, 2, 4], $n))), PHP_EOL;
  echo rtrim('     Lucas: ' . show(gen([2, 1], $n))), PHP_EOL;
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
