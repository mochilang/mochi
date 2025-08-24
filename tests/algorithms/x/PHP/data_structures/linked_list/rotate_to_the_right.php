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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function list_to_string($xs) {
  if (count($xs) == 0) {
  return '';
}
  $s = _str($xs[0]);
  $i = 1;
  while ($i < count($xs)) {
  $s = $s . '->' . _str($xs[$i]);
  $i = $i + 1;
};
  return $s;
};
  function insert_node($xs, $data) {
  return _append($xs, $data);
};
  function rotate_to_the_right($xs, $places) {
  if (count($xs) == 0) {
  _panic('The linked list is empty.');
}
  $n = count($xs);
  $k = $places % $n;
  if ($k == 0) {
  return $xs;
}
  $split = $n - $k;
  $res = [];
  $i = $split;
  while ($i < $n) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  $j = 0;
  while ($j < $split) {
  $res = _append($res, $xs[$j]);
  $j = $j + 1;
};
  return $res;
};
  function main() {
  $head = [];
  $head = insert_node($head, 5);
  $head = insert_node($head, 1);
  $head = insert_node($head, 2);
  $head = insert_node($head, 4);
  $head = insert_node($head, 3);
  echo rtrim('Original list: ' . list_to_string($head)), PHP_EOL;
  $places = 3;
  $new_head = rotate_to_the_right($head, $places);
  echo rtrim('After ' . _str($places) . ' iterations: ' . list_to_string($new_head)), PHP_EOL;
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
