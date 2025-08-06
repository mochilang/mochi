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
$NIL = 0 - 1;
$nodes = [];
function make_linked_list($elements) {
  global $NIL, $nodes;
  if (count($elements) == 0) {
  $panic('The Elements List is empty');
}
  $nodes = [];
  $nodes = _append($nodes, ['data' => $elements[0], 'next' => $NIL]);
  $head = 0;
  $current = $head;
  $i = 1;
  while ($i < count($elements)) {
  $nodes = _append($nodes, ['data' => $elements[$i], 'next' => $NIL]);
  $nodes[$current]['next'] = count($nodes) - 1;
  $current = count($nodes) - 1;
  $i = $i + 1;
};
  return $head;
}
function node_to_string($head) {
  global $NIL, $nodes;
  $s = '';
  $index = $head;
  while ($index != $NIL) {
  $node = $nodes[$index];
  $s = $s . '<' . _str($node['data']) . '> ---> ';
  $index = $node['next'];
};
  $s = $s . '<END>';
  return $s;
}
function main() {
  global $NIL, $nodes;
  $list_data = [1, 3, 5, 32, 44, 12, 43];
  echo rtrim('List: ' . _str($list_data)), PHP_EOL;
  echo rtrim('Creating Linked List from List.'), PHP_EOL;
  $head = make_linked_list($list_data);
  echo rtrim('Linked List:'), PHP_EOL;
  echo rtrim(node_to_string($head)), PHP_EOL;
}
main();
