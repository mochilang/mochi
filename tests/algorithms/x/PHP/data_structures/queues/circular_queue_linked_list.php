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
  function create_queue($capacity) {
  $data = [];
  $next = [];
  $prev = [];
  $i = 0;
  while ($i < $capacity) {
  $data = _append($data, '');
  $next = _append($next, ($i + 1) % $capacity);
  $prev = _append($prev, ($i - 1 + $capacity) % $capacity);
  $i = $i + 1;
};
  return ['data' => $data, 'front' => 0, 'next' => $next, 'prev' => $prev, 'rear' => 0];
};
  function is_empty($q) {
  return $q['front'] == $q['rear'] && $q['data'][$q['front']] == '';
};
  function check_can_perform($q) {
  if (is_empty($q)) {
  _panic('Empty Queue');
}
};
  function check_is_full($q) {
  if ($q['next'][$q['rear']] == $q['front']) {
  _panic('Full Queue');
}
};
  function peek($q) {
  check_can_perform($q);
  return $q['data'][$q['front']];
};
  function enqueue($q, $value) {
  check_is_full($q);
  if (!is_empty($q)) {
  $q['rear'] = $q['next'][$q['rear']];
}
  $data = $q['data'];
  $data[$q['rear']] = $value;
  $q['data'] = $data;
  return $q;
};
  function dequeue(&$q) {
  check_can_perform($q);
  $data = $q['data'];
  $val = $data[$q['front']];
  $data[$q['front']] = '';
  $q['data'] = $data;
  if ($q['front'] != $q['rear']) {
  $q['front'] = $q['next'][$q['front']];
}
  return ['queue' => $q, 'value' => $val];
};
  function main() {
  $q = create_queue(3);
  echo rtrim(_str(is_empty($q))), PHP_EOL;
  $q = enqueue($q, 'a');
  $q = enqueue($q, 'b');
  echo rtrim(peek($q)), PHP_EOL;
  $res = dequeue($q);
  $q = $res['queue'];
  echo rtrim($res['value']), PHP_EOL;
  $res = dequeue($q);
  $q = $res['queue'];
  echo rtrim($res['value']), PHP_EOL;
  echo rtrim(_str(is_empty($q))), PHP_EOL;
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
