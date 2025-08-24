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
  function make_stack() {
  global $stack;
  return ['main_queue' => [], 'temp_queue' => []];
};
  function push(&$s, $item) {
  global $stack;
  $s['temp_queue'] = _append($s['temp_queue'], $item);
  while (_len($s['main_queue']) > 0) {
  $s['temp_queue'] = _append($s['temp_queue'], $s['main_queue'][0]);
  $s['main_queue'] = array_slice($s['main_queue'], 1, _len($s['main_queue']) - 1);
};
  $new_main = $s['temp_queue'];
  $s['temp_queue'] = $s['main_queue'];
  $s['main_queue'] = $new_main;
};
  function pop(&$s) {
  global $stack;
  if (_len($s['main_queue']) == 0) {
  _panic('pop from empty stack');
}
  $item = $s['main_queue'][0];
  $s['main_queue'] = array_slice($s['main_queue'], 1, _len($s['main_queue']) - 1);
  return $item;
};
  function peek($s) {
  global $stack;
  if (_len($s['main_queue']) == 0) {
  _panic('peek from empty stack');
}
  return $s['main_queue'][0];
};
  function is_empty($s) {
  global $stack;
  return _len($s['main_queue']) == 0;
};
  $stack = make_stack();
  push($stack, 1);
  push($stack, 2);
  push($stack, 3);
  echo rtrim(_str(peek($stack))), PHP_EOL;
  echo rtrim(_str(pop($stack))), PHP_EOL;
  echo rtrim(_str(peek($stack))), PHP_EOL;
  echo rtrim(_str(pop($stack))), PHP_EOL;
  echo rtrim(_str(pop($stack))), PHP_EOL;
  echo rtrim(_str(is_empty($stack))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
