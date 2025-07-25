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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
  function mean($v) {
  if (count($v) == 0) {
  return ['ok' => false];
}
  $sum = 0.0;
  $i = 0;
  while ($i < count($v)) {
  $sum = $sum + $v[$i];
  $i = $i + 1;
};
  return ['ok' => true, 'mean' => $sum / (floatval(count($v)))];
};
  function main() {
  $sets = [[], [3.0, 1.0, 4.0, 1.0, 5.0, 9.0], [100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, -100000000000000000000.0], [10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11], [10.0, 20.0, 30.0, 40.0, 50.0, -100.0, 4.7, -1100.0]];
  foreach ($sets as $v) {
  echo rtrim('Vector: ' . _str($v)), PHP_EOL;
  $r = mean($v);
  if ($r['ok']) {
  echo rtrim('Mean of ' . _str(_len($v)) . ' numbers is ' . _str($r['mean'])), PHP_EOL;
} else {
  echo rtrim('Mean undefined'), PHP_EOL;
}
  echo rtrim(''), PHP_EOL;
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
