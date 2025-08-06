<?php
ini_set('memory_limit', '-1');
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
function new_deque() {
  $nodes = [];
  $nodes = _append($nodes, ['data' => '', 'prev' => -1, 'next' => 1]);
  $nodes = _append($nodes, ['data' => '', 'prev' => 0, 'next' => -1]);
  return ['nodes' => $nodes, 'header' => 0, 'trailer' => 1, 'size' => 0];
}
function is_empty($d) {
  return $d['size'] == 0;
}
function front($d) {
  if (is_empty($d)) {
  $panic('List is empty');
}
  $head = $d['nodes'][$d['header']];
  $idx = $head['next'];
  $node = $d['nodes'][$idx];
  return $node['data'];
}
function back($d) {
  if (is_empty($d)) {
  $panic('List is empty');
}
  $tail = $d['nodes'][$d['trailer']];
  $idx = $tail['prev'];
  $node = $d['nodes'][$idx];
  return $node['data'];
}
function insert(&$d, $pred, $value, $succ) {
  $nodes = $d['nodes'];
  $new_idx = count($nodes);
  $nodes = _append($nodes, ['data' => $value, 'prev' => $pred, 'next' => $succ]);
  $pred_node = $nodes[$pred];
  $pred_node['next'] = $new_idx;
  $nodes[$pred] = $pred_node;
  $succ_node = $nodes[$succ];
  $succ_node['prev'] = $new_idx;
  $nodes[$succ] = $succ_node;
  $d['nodes'] = $nodes;
  $d['size'] = $d['size'] + 1;
  return $d;
}
function delete(&$d, $idx) {
  $nodes = $d['nodes'];
  $node = $nodes[$idx];
  $pred = $node['prev'];
  $succ = $node['next'];
  $pred_node = $nodes[$pred];
  $pred_node['next'] = $succ;
  $nodes[$pred] = $pred_node;
  $succ_node = $nodes[$succ];
  $succ_node['prev'] = $pred;
  $nodes[$succ] = $succ_node;
  $val = $node['data'];
  $d['nodes'] = $nodes;
  $d['size'] = $d['size'] - 1;
  return ['deque' => $d, 'value' => $val];
}
function add_first(&$d, $value) {
  $head = $d['nodes'][$d['header']];
  $succ = $head['next'];
  return insert($d, $d['header'], $value, $succ);
}
function add_last(&$d, $value) {
  $tail = $d['nodes'][$d['trailer']];
  $pred = $tail['prev'];
  return insert($d, $pred, $value, $d['trailer']);
}
function remove_first(&$d) {
  if (is_empty($d)) {
  $panic('remove_first from empty list');
}
  $head = $d['nodes'][$d['header']];
  $idx = $head['next'];
  return delete($d, $idx);
}
function remove_last(&$d) {
  if (is_empty($d)) {
  $panic('remove_first from empty list');
}
  $tail = $d['nodes'][$d['trailer']];
  $idx = $tail['prev'];
  return delete($d, $idx);
}
function main() {
  $d = new_deque();
  $d = add_first($d, 'A');
  echo rtrim(front($d)), PHP_EOL;
  $d = add_last($d, 'B');
  echo rtrim(back($d)), PHP_EOL;
  $r = remove_first($d);
  $d = $r['deque'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  $r = remove_last($d);
  $d = $r['deque'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  echo rtrim(_str(is_empty($d))), PHP_EOL;
}
main();
