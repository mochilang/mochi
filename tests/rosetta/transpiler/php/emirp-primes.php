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
  function revInt($n) {
  $r = 0;
  $t = $n;
  while ($t > 0) {
  $r = $r * 10 + $t % 10;
  $t = intval((_intdiv($t, 10)));
};
  return $r;
};
  function main() {
  $emirps = [];
  $n = 2;
  while (count($emirps) < 10000) {
  if (isPrime($n)) {
  $r = revInt($n);
  if ($r != $n && isPrime($r)) {
  $emirps = array_merge($emirps, [$n]);
};
}
  $n = $n + 1;
};
  $line = '   [';
  $i = 0;
  while ($i < 20) {
  $line = $line . _str($emirps[$i]);
  if ($i < 19) {
  $line = $line . ', ';
}
  $i = $i + 1;
};
  $line = $line . ']';
  echo rtrim('First 20:'), PHP_EOL;
  echo rtrim($line), PHP_EOL;
  $line = '  [';
  foreach ($emirps as $e) {
  if ($e >= 8000) {
  break;
}
  if ($e >= 7700) {
  $line = $line . _str($e) . ', ';
}
};
  $line = $line . ']';
  echo rtrim('Between 7700 and 8000:'), PHP_EOL;
  echo rtrim($line), PHP_EOL;
  echo rtrim('10000th:'), PHP_EOL;
  echo rtrim('   [' . _str($emirps[9999]) . ']'), PHP_EOL;
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
