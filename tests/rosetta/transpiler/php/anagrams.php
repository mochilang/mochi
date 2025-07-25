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
$__start_mem = memory_get_usage();
$__start = _now();
  function sortRunes($s) {
  $arr = [];
  $i = 0;
  while ($i < strlen($s)) {
  $arr = array_merge($arr, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $n = count($arr);
  $m = 0;
  while ($m < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($arr[$j] > $arr[$j + 1]) {
  $tmp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $m = $m + 1;
};
  $out = '';
  $i = 0;
  while ($i < $n) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return $out;
};
  function sortStrings($xs) {
  $res = [];
  $tmp = $xs;
  while (count($tmp) > 0) {
  $min = $tmp[0];
  $idx = 0;
  $i = 1;
  while ($i < count($tmp)) {
  if ($tmp[$i] < $min) {
  $min = $tmp[$i];
  $idx = $i;
}
  $i = $i + 1;
};
  $res = array_merge($res, [$min]);
  $out = [];
  $j = 0;
  while ($j < count($tmp)) {
  if ($j != $idx) {
  $out = array_merge($out, [$tmp[$j]]);
}
  $j = $j + 1;
};
  $tmp = $out;
};
  return $res;
};
  function main() {
  $words = ['abel', 'able', 'bale', 'bela', 'elba', 'alger', 'glare', 'lager', 'large', 'regal', 'angel', 'angle', 'galen', 'glean', 'lange', 'caret', 'carte', 'cater', 'crate', 'trace', 'elan', 'lane', 'lean', 'lena', 'neal', 'evil', 'levi', 'live', 'veil', 'vile'];
  $groups = [];
  $maxLen = 0;
  foreach ($words as $w) {
  $k = sortRunes($w);
  if (!(array_key_exists($k, $groups))) {
  $groups[$k] = [$w];
} else {
  $groups[$k] = array_merge($groups[$k], [$w]);
}
  if (count($groups[$k]) > $maxLen) {
  $maxLen = count($groups[$k]);
}
};
  $printed = [];
  foreach ($words as $w) {
  $k = sortRunes($w);
  if (count($groups[$k]) == $maxLen) {
  if (!(array_key_exists($k, $printed))) {
  $g = sortStrings($groups[$k]);
  $line = '[' . $g[0];
  $i = 1;
  while ($i < count($g)) {
  $line = $line . ' ' . $g[$i];
  $i = $i + 1;
};
  $line = $line . ']';
  echo rtrim($line), PHP_EOL;
  $printed[$k] = true;
};
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
