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
  function factorial($n) {
  $result = 1;
  $i = 2;
  while ($i <= $n) {
  $result = $result * $i;
  $i = $i + 1;
};
  return $result;
};
  function isPrime($n) {
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  $d = 3;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
};
  return true;
};
  function padLeft($s, $w) {
  $out = $s;
  while (strlen($out) < $w) {
  $out = ' ' . $out;
};
  return $out;
};
  function main() {
  $n = 0;
  $count = 0;
  while ($count < 10) {
  $n = $n + 1;
  $f = factorial($n);
  if (isPrime($f - 1)) {
  $count = $count + 1;
  echo rtrim(padLeft(_str($count), 2) . ': ' . padLeft(_str($n), 2) . '! - 1 = ' . _str($f - 1)), PHP_EOL;
}
  if ($count < 10 && isPrime($f + 1)) {
  $count = $count + 1;
  echo rtrim(padLeft(_str($count), 2) . ': ' . padLeft(_str($n), 2) . '! + 1 = ' . _str($f + 1)), PHP_EOL;
}
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
