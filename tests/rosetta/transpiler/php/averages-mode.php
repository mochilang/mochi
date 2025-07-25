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
  $arr1 = [2, 7, 1, 8, 2];
  $counts1 = [];
  $keys1 = [];
  $i = 0;
  while ($i < count($arr1)) {
  $v = $arr1[$i];
  if (array_key_exists($v, $counts1)) {
  $counts1[$v] = $counts1[$v] + 1;
} else {
  $counts1[$v] = 1;
  $keys1 = array_merge($keys1, [$v]);
}
  $i = $i + 1;
}
  $max1 = 0;
  $i = 0;
  while ($i < count($keys1)) {
  $k = $keys1[$i];
  $c = $counts1[$k];
  if ($c > $max1) {
  $max1 = $c;
}
  $i = $i + 1;
}
  $modes1 = [];
  $i = 0;
  while ($i < count($keys1)) {
  $k = $keys1[$i];
  if ($counts1[$k] == $max1) {
  $modes1 = array_merge($modes1, [$k]);
}
  $i = $i + 1;
}
  echo rtrim(_str($modes1)), PHP_EOL;
  $arr2 = [2, 7, 1, 8, 2, 8];
  $counts2 = [];
  $keys2 = [];
  $i = 0;
  while ($i < count($arr2)) {
  $v = $arr2[$i];
  if (array_key_exists($v, $counts2)) {
  $counts2[$v] = $counts2[$v] + 1;
} else {
  $counts2[$v] = 1;
  $keys2 = array_merge($keys2, [$v]);
}
  $i = $i + 1;
}
  $max2 = 0;
  $i = 0;
  while ($i < count($keys2)) {
  $k = $keys2[$i];
  $c = $counts2[$k];
  if ($c > $max2) {
  $max2 = $c;
}
  $i = $i + 1;
}
  $modes2 = [];
  $i = 0;
  while ($i < count($keys2)) {
  $k = $keys2[$i];
  if ($counts2[$k] == $max2) {
  $modes2 = array_merge($modes2, [$k]);
}
  $i = $i + 1;
}
  echo rtrim(_str($modes2)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;
