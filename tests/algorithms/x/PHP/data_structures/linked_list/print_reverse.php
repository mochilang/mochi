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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function empty_list() {
  return ['data' => []];
};
  function append_value($list, $value) {
  $d = $list['data'];
  $d = _append($d, $value);
  return ['data' => $d];
};
  function extend_list($list, $items) {
  $result = $list;
  $i = 0;
  while ($i < count($items)) {
  $result = append_value($result, $items[$i]);
  $i = $i + 1;
};
  return $result;
};
  function to_string($list) {
  if (_len($list['data']) == 0) {
  return '';
}
  $s = _str($list['data'][0]);
  $i = 1;
  while ($i < _len($list['data'])) {
  $s = $s . ' -> ' . _str($list['data'][$i]);
  $i = $i + 1;
};
  return $s;
};
  function make_linked_list($items) {
  if (count($items) == 0) {
  _panic('The Elements List is empty');
}
  $ll = empty_list();
  $ll = extend_list($ll, $items);
  return $ll;
};
  function in_reverse($list) {
  if (_len($list['data']) == 0) {
  return '';
}
  $i = _len($list['data']) - 1;
  $s = _str($list['data'][$i]);
  $i = $i - 1;
  while ($i >= 0) {
  $s = $s . ' <- ' . _str($list['data'][$i]);
  $i = $i - 1;
};
  return $s;
};
  function main() {
  $linked_list = make_linked_list([14, 52, 14, 12, 43]);
  echo rtrim('Linked List:  ' . to_string($linked_list)), PHP_EOL;
  echo rtrim('Reverse List: ' . in_reverse($linked_list)), PHP_EOL;
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
