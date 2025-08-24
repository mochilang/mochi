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
  function empty_deque() {
  return ['data' => []];
};
  function push_back($dq, $value) {
  return ['data' => _append($dq['data'], $value)];
};
  function push_front($dq, $value) {
  $res = [$value];
  $i = 0;
  while ($i < _len($dq['data'])) {
  $res = _append($res, $dq['data'][$i]);
  $i = $i + 1;
};
  return ['data' => $res];
};
  function extend_back($dq, $values) {
  $res = $dq['data'];
  $i = 0;
  while ($i < count($values)) {
  $res = _append($res, $values[$i]);
  $i = $i + 1;
};
  return ['data' => $res];
};
  function extend_front($dq, $values) {
  $res = [];
  $i = count($values) - 1;
  while ($i >= 0) {
  $res = _append($res, $values[$i]);
  $i = $i - 1;
};
  $j = 0;
  while ($j < _len($dq['data'])) {
  $res = _append($res, $dq['data'][$j]);
  $j = $j + 1;
};
  return ['data' => $res];
};
  function pop_back($dq) {
  if (_len($dq['data']) == 0) {
  _panic('pop from empty deque');
}
  $res = [];
  $i = 0;
  while ($i < _len($dq['data']) - 1) {
  $res = _append($res, $dq['data'][$i]);
  $i = $i + 1;
};
  return ['deque' => ['data' => $res], 'value' => $dq['data'][_len($dq['data']) - 1]];
};
  function pop_front($dq) {
  if (_len($dq['data']) == 0) {
  _panic('popleft from empty deque');
}
  $res = [];
  $i = 1;
  while ($i < _len($dq['data'])) {
  $res = _append($res, $dq['data'][$i]);
  $i = $i + 1;
};
  return ['deque' => ['data' => $res], 'value' => $dq['data'][0]];
};
  function is_empty($dq) {
  return _len($dq['data']) == 0;
};
  function length($dq) {
  return _len($dq['data']);
};
  function to_string($dq) {
  if (_len($dq['data']) == 0) {
  return '[]';
}
  $s = '[' . _str($dq['data'][0]);
  $i = 1;
  while ($i < _len($dq['data'])) {
  $s = $s . ', ' . _str($dq['data'][$i]);
  $i = $i + 1;
};
  return $s . ']';
};
  function main() {
  $dq = empty_deque();
  $dq = push_back($dq, 2);
  $dq = push_front($dq, 1);
  $dq = extend_back($dq, [3, 4]);
  $dq = extend_front($dq, [0]);
  echo rtrim(to_string($dq)), PHP_EOL;
  $r = pop_back($dq);
  $dq = $r['deque'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  $r = pop_front($dq);
  $dq = $r['deque'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($dq)), PHP_EOL;
  echo rtrim(json_encode(is_empty(empty_deque()), 1344)), PHP_EOL;
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
