<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function get_digits($num) {
  $cube = $num * $num * $num;
  $s = _str($cube);
  $counts = [];
  $j = 0;
  while ($j < 10) {
  $counts = _append($counts, 0);
  $j = $j + 1;
};
  $i = 0;
  while ($i < strlen($s)) {
  $d = intval(substr($s, $i, $i + 1 - $i));
  $counts[$d] = $counts[$d] + 1;
  $i = $i + 1;
};
  $result = '';
  $d = 0;
  while ($d < 10) {
  $c = $counts[$d];
  while ($c > 0) {
  $result = $result . _str($d);
  $c = $c - 1;
};
  $d = $d + 1;
};
  return $result;
};
  function solution($max_base) {
  $freqs = [];
  $num = 0;
  while (true) {
  $digits = get_digits($num);
  $arr = [];
  if (array_key_exists($digits, $freqs)) {
  $arr = $freqs[$digits];
}
  $arr = _append($arr, $num);
  $freqs[$digits] = $arr;
  if (count($arr) == $max_base) {
  $base = $arr[0];
  return $base * $base * $base;
}
  $num = $num + 1;
};
};
  echo rtrim('solution() = ' . _str(solution(5))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
