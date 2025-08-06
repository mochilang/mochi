<?php
ini_set('memory_limit', '-1');
function _len($x) {
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
function new_queue() {
  global $queue;
  return ['nodes' => [], 'front' => 0 - 1, 'rear' => 0 - 1];
}
function is_empty($q) {
  global $queue;
  return $q['front'] == 0 - 1;
}
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
}
function get(&$q) {
  global $queue;
  if (is_empty($q)) {
  $panic('dequeue from empty queue');
}
  $idx = $q['front'];
  $node = $q['nodes'][$idx];
  $q['front'] = $node['next'];
  if ($q['front'] == 0 - 1) {
  $q['rear'] = 0 - 1;
}
  return $node['data'];
}
function length($q) {
  global $queue;
  $count = 0;
  $idx = $q['front'];
  while ($idx != 0 - 1) {
  $count = $count + 1;
  $idx = $q['nodes'][$idx]['next'];
};
  return $count;
}
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
}
function clear(&$q) {
  global $queue;
  $q['nodes'] = [];
  $q['front'] = 0 - 1;
  $q['rear'] = 0 - 1;
}
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
