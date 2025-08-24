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
  function make_stack($limit) {
  return ['items' => [], 'limit' => $limit];
};
  function is_empty($s) {
  return _len($s['items']) == 0;
};
  function size($s) {
  return _len($s['items']);
};
  function is_full($s) {
  return _len($s['items']) >= $s['limit'];
};
  function push(&$s, $item) {
  if (is_full($s)) {
  _panic('stack overflow');
}
  $s['items'] = _append($s['items'], $item);
};
  function pop(&$s) {
  if (is_empty($s)) {
  _panic('stack underflow');
}
  $n = _len($s['items']);
  $val = $s['items'][$n - 1];
  $s['items'] = array_slice($s['items'], 0, $n - 1);
  return $val;
};
  function peek($s) {
  if (is_empty($s)) {
  _panic('peek from empty stack');
}
  return $s['items'][_len($s['items']) - 1];
};
  function mochi_contains($s, $item) {
  $i = 0;
  while ($i < _len($s['items'])) {
  if ($s['items'][$i] == $item) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function stack_repr($s) {
  return _str($s['items']);
};
  function main() {
  $s = make_stack(5);
  echo rtrim(_str(is_empty($s))), PHP_EOL;
  push($s, 0);
  push($s, 1);
  push($s, 2);
  echo rtrim(_str(peek($s))), PHP_EOL;
  echo rtrim(_str(size($s))), PHP_EOL;
  echo rtrim(_str(is_full($s))), PHP_EOL;
  push($s, 3);
  push($s, 4);
  echo rtrim(_str(is_full($s))), PHP_EOL;
  echo rtrim(stack_repr($s)), PHP_EOL;
  echo rtrim(_str(pop($s))), PHP_EOL;
  echo rtrim(_str(peek($s))), PHP_EOL;
  echo rtrim(_str(mochi_contains($s, 1))), PHP_EOL;
  echo rtrim(_str(mochi_contains($s, 9))), PHP_EOL;
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
