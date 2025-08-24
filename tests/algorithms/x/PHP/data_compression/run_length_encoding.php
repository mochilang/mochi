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
$__start_mem = memory_get_usage();
$__start = _now();
  function run_length_encode($text) {
  global $encoded1, $encoded2, $encoded3, $example1, $example2, $example3;
  if (strlen($text) == 0) {
  return '';
}
  $encoded = '';
  $count = 1;
  $i = 0;
  while ($i < strlen($text)) {
  if ($i + 1 < strlen($text) && substr($text, $i, $i + 1 - $i) == substr($text, $i + 1, $i + 1 + 1 - ($i + 1))) {
  $count = $count + 1;
} else {
  $encoded = $encoded . substr($text, $i, $i + 1 - $i) . _str($count);
  $count = 1;
}
  $i = $i + 1;
};
  return $encoded;
};
  function run_length_decode($encoded) {
  global $encoded1, $encoded2, $encoded3, $example1, $example2, $example3;
  $res = '';
  $i = 0;
  while ($i < strlen($encoded)) {
  $ch = substr($encoded, $i, $i + 1 - $i);
  $i = $i + 1;
  $num_str = '';
  while ($i < strlen($encoded) && substr($encoded, $i, $i + 1 - $i) >= '0' && substr($encoded, $i, $i + 1 - $i) <= '9') {
  $num_str = $num_str . substr($encoded, $i, $i + 1 - $i);
  $i = $i + 1;
};
  $count = intval($num_str);
  $j = 0;
  while ($j < $count) {
  $res = $res . $ch;
  $j = $j + 1;
};
};
  return $res;
};
  $example1 = 'AAAABBBCCDAA';
  $encoded1 = run_length_encode($example1);
  echo rtrim($encoded1), PHP_EOL;
  echo rtrim(run_length_decode($encoded1)), PHP_EOL;
  $example2 = 'A';
  $encoded2 = run_length_encode($example2);
  echo rtrim($encoded2), PHP_EOL;
  echo rtrim(run_length_decode($encoded2)), PHP_EOL;
  $example3 = 'AAADDDDDDFFFCCCAAVVVV';
  $encoded3 = run_length_encode($example3);
  echo rtrim($encoded3), PHP_EOL;
  echo rtrim(run_length_decode($encoded3)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
