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
  function new_suffix_tree($text) {
  return ['text' => $text];
};
  function search($tree, $pattern) {
  $n = strlen($tree['text']);
  $m = strlen($pattern);
  if ($m == 0) {
  return true;
}
  if ($m > $n) {
  return false;
}
  $i = 0;
  while ($i <= $n - $m) {
  if (substr($tree['text'], $i, $i + $m - $i) == $pattern) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function main() {
  $text = 'monkey banana';
  $suffix_tree = new_suffix_tree($text);
  $patterns = ['ana', 'ban', 'na', 'xyz', 'mon'];
  $i = 0;
  while ($i < count($patterns)) {
  $pattern = $patterns[$i];
  $found = search($suffix_tree, $pattern);
  echo rtrim('Pattern \'' . $pattern . '\' found: ' . _str($found)), PHP_EOL;
  $i = $i + 1;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
