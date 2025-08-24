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
  function new_queue() {
  global $queue;
  return ['front' => 0 - 1, 'nodes' => [], 'rear' => 0 - 1];
};
  function is_empty($q) {
  global $queue;
  return $q['front'] == 0 - 1;
};
  function put(&$q, $item) {
  global $queue;
  $node = ['data' => $item, 'next' => 0 - 1];
  $q['nodes'] = _append($q['nodes'], $node);
  $idx = _len($q['nodes']) - 1;
  if ($q['front'] == 0 - 1) {
  $q['front'] = $idx;
  $q['rear'] = $idx;
} else {
  $nodes = $q['nodes'];
  $nodes[$q['rear']]['next'] = $idx;
  $q['nodes'] = $nodes;
  $q['rear'] = $idx;
}
};
  function get(&$q) {
  global $queue;
  if (is_empty($q)) {
  _panic('dequeue from empty queue');
}
  $idx = $q['front'];
  $node = $q['nodes'][$idx];
  $q['front'] = $node['next'];
  if ($q['front'] == 0 - 1) {
  $q['rear'] = 0 - 1;
}
  return $node['data'];
};
  function length($q) {
  global $queue;
  $count = 0;
  $idx = $q['front'];
  while ($idx != 0 - 1) {
  $count = $count + 1;
  $idx = $q['nodes'][$idx]['next'];
};
  return $count;
};
  function to_string($q) {
  global $queue;
  $res = '';
  $idx = $q['front'];
  $first = true;
  while ($idx != 0 - 1) {
  $node = $q['nodes'][$idx];
  if ($first) {
  $res = $node['data'];
  $first = false;
} else {
  $res = $res . ' <- ' . $node['data'];
}
  $idx = $node['next'];
};
  return $res;
};
  function clear(&$q) {
  global $queue;
  $q['nodes'] = [];
  $q['front'] = 0 - 1;
  $q['rear'] = 0 - 1;
};
  $queue = new_queue();
  echo rtrim(_str(is_empty($queue))), PHP_EOL;
  put($queue, '5');
  put($queue, '9');
  put($queue, 'python');
  echo rtrim(_str(is_empty($queue))), PHP_EOL;
  echo rtrim(get($queue)), PHP_EOL;
  put($queue, 'algorithms');
  echo rtrim(get($queue)), PHP_EOL;
  echo rtrim(get($queue)), PHP_EOL;
  echo rtrim(get($queue)), PHP_EOL;
  echo rtrim(_str(is_empty($queue))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
