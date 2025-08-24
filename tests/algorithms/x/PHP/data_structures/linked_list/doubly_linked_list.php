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
  function empty_list() {
  return ['data' => []];
};
  function length($list) {
  return count($list['data']);
};
  function is_empty($list) {
  return count($list['data']) == 0;
};
  function to_string($list) {
  if (count($list['data']) == 0) {
  return '';
}
  $s = _str($list['data'][0]);
  $i = 1;
  while ($i < count($list['data'])) {
  $s = $s . '->' . _str($list['data'][$i]);
  $i = $i + 1;
};
  return $s;
};
  function insert_nth($list, $index, $value) {
  if ($index < 0 || $index > count($list['data'])) {
  _panic('index out of range');
}
  $res = [];
  $i = 0;
  while ($i < $index) {
  $res = _append($res, $list['data'][$i]);
  $i = $i + 1;
};
  $res = _append($res, $value);
  while ($i < count($list['data'])) {
  $res = _append($res, $list['data'][$i]);
  $i = $i + 1;
};
  return ['data' => $res];
};
  function insert_head($list, $value) {
  return insert_nth($list, 0, $value);
};
  function insert_tail($list, $value) {
  return insert_nth($list, count($list['data']), $value);
};
  function delete_nth($list, $index) {
  if ($index < 0 || $index >= count($list['data'])) {
  _panic('index out of range');
}
  $res = [];
  $i = 0;
  $removed = 0;
  while ($i < count($list['data'])) {
  if ($i == $index) {
  $removed = $list['data'][$i];
} else {
  $res = _append($res, $list['data'][$i]);
}
  $i = $i + 1;
};
  return ['list' => ['data' => $res], 'value' => $removed];
};
  function delete_head($list) {
  return delete_nth($list, 0);
};
  function delete_tail($list) {
  return delete_nth($list, count($list['data']) - 1);
};
  function delete_value($list, $value) {
  $idx = 0;
  $found = false;
  while ($idx < count($list['data'])) {
  if ($list['data'][$idx] == $value) {
  $found = true;
  break;
}
  $idx = $idx + 1;
};
  if (!$found) {
  _panic('value not found');
}
  return delete_nth($list, $idx);
};
  function main() {
  $dll = empty_list();
  $dll = insert_tail($dll, 1);
  $dll = insert_tail($dll, 2);
  $dll = insert_tail($dll, 3);
  echo rtrim(to_string($dll)), PHP_EOL;
  $dll = insert_head($dll, 0);
  echo rtrim(to_string($dll)), PHP_EOL;
  $dll = insert_nth($dll, 2, 9);
  echo rtrim(to_string($dll)), PHP_EOL;
  $res = delete_nth($dll, 2);
  $dll = $res['list'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($dll)), PHP_EOL;
  $res = delete_tail($dll);
  $dll = $res['list'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($dll)), PHP_EOL;
  $res = delete_value($dll, 1);
  $dll = $res['list'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($dll)), PHP_EOL;
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
