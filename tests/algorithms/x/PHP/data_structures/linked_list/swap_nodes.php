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
function _len($x) {
    if ($x === null) { return 0; }
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
  function empty_list() {
  return ['data' => []];
};
  function push($list, $value) {
  $res = [$value];
  $res = array_merge($res, $list['data']);
  return ['data' => $res];
};
  function swap_nodes($list, $v1, $v2) {
  if ($v1 == $v2) {
  return $list;
}
  $idx1 = 0 - 1;
  $idx2 = 0 - 1;
  $i = 0;
  while ($i < _len($list['data'])) {
  if ($list['data'][$i] == $v1 && $idx1 == 0 - 1) {
  $idx1 = $i;
}
  if ($list['data'][$i] == $v2 && $idx2 == 0 - 1) {
  $idx2 = $i;
}
  $i = $i + 1;
};
  if ($idx1 == 0 - 1 || $idx2 == 0 - 1) {
  return $list;
}
  $res = $list['data'];
  $temp = $res[$idx1];
  $res[$idx1] = $res[$idx2];
  $res[$idx2] = $temp;
  return ['data' => $res];
};
  function to_string($list) {
  return _str($list['data']);
};
  function main() {
  $ll = empty_list();
  $i = 5;
  while ($i > 0) {
  $ll = push($ll, $i);
  $i = $i - 1;
};
  echo rtrim('Original Linked List: ' . to_string($ll)), PHP_EOL;
  $ll = swap_nodes($ll, 1, 4);
  echo rtrim('Modified Linked List: ' . to_string($ll)), PHP_EOL;
  echo rtrim('After swapping the nodes whose data is 1 and 4.'), PHP_EOL;
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
