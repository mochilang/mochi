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
  function to_string($list) {
  if (count($list['data']) == 0) {
  return '';
}
  $s = _str($list['data'][0]);
  $i = 1;
  while ($i < count($list['data'])) {
  $s = $s . ' -> ' . _str($list['data'][$i]);
  $i = $i + 1;
};
  return $s;
};
  function reverse_k_nodes($list, $k) {
  if ($k <= 1) {
  return $list;
}
  $res = [];
  $i = 0;
  while ($i < count($list['data'])) {
  $j = 0;
  $group = [];
  while ($j < $k && $i + $j < count($list['data'])) {
  $group = _append($group, $list['data'][$i + $j]);
  $j = $j + 1;
};
  if (count($group) == $k) {
  $g = $k - 1;
  while ($g >= 0) {
  $res = _append($res, $group[$g]);
  $g = $g - 1;
};
} else {
  $g = 0;
  while ($g < count($group)) {
  $res = _append($res, $group[$g]);
  $g = $g + 1;
};
}
  $i = $i + $k;
};
  return ['data' => $res];
};
  function main() {
  $ll = ['data' => [1, 2, 3, 4, 5]];
  echo rtrim('Original Linked List: ' . to_string($ll)), PHP_EOL;
  $k = 2;
  $ll = reverse_k_nodes($ll, $k);
  echo rtrim('After reversing groups of size ' . _str($k) . ': ' . to_string($ll)), PHP_EOL;
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
