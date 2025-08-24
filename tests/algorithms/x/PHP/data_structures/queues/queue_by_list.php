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
  function new_queue($items) {
  global $front, $q, $res;
  return ['entries' => $items];
};
  function len_queue($q) {
  global $front, $res;
  return count($q['entries']);
};
  function str_queue($q) {
  global $front, $res;
  $s = 'Queue((';
  $i = 0;
  while ($i < count($q['entries'])) {
  $s = $s . _str($q['entries'][$i]);
  if ($i < count($q['entries']) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . '))';
  return $s;
};
  function put($q, $item) {
  global $front, $res;
  $e = $q['entries'];
  $e = _append($e, $item);
  return ['entries' => $e];
};
  function get($q) {
  global $front, $res;
  if (count($q['entries']) == 0) {
  _panic('Queue is empty');
}
  $value = $q['entries'][0];
  $new_entries = [];
  $i = 1;
  while ($i < count($q['entries'])) {
  $new_entries = _append($new_entries, $q['entries'][$i]);
  $i = $i + 1;
};
  return ['queue' => ['entries' => $new_entries], 'value' => $value];
};
  function rotate($q, $rotation) {
  global $front, $res;
  $e = $q['entries'];
  $r = 0;
  while ($r < $rotation) {
  if (count($e) > 0) {
  $first = $e[0];
  $rest = [];
  $i = 1;
  while ($i < count($e)) {
  $rest = _append($rest, $e[$i]);
  $i = $i + 1;
};
  $rest = _append($rest, $first);
  $e = $rest;
}
  $r = $r + 1;
};
  return ['entries' => $e];
};
  function get_front($q) {
  global $front, $res;
  return $q['entries'][0];
};
  $q = new_queue([]);
  echo rtrim(json_encode(len_queue($q), 1344)), PHP_EOL;
  $q = put($q, 10);
  $q = put($q, 20);
  $q = put($q, 30);
  $q = put($q, 40);
  echo rtrim(str_queue($q)), PHP_EOL;
  $res = get($q);
  $q = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(str_queue($q)), PHP_EOL;
  $q = rotate($q, 2);
  echo rtrim(str_queue($q)), PHP_EOL;
  $front = get_front($q);
  echo rtrim(json_encode($front, 1344)), PHP_EOL;
  echo rtrim(str_queue($q)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
