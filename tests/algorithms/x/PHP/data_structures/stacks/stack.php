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
function make_stack($limit) {
  return ['items' => [], 'limit' => $limit];
}
function is_empty($s) {
  return _len($s['items']) == 0;
}
function size($s) {
  return _len($s['items']);
}
function is_full($s) {
  return _len($s['items']) >= $s['limit'];
}
function push(&$s, $item) {
  if (is_full($s)) {
  $panic('stack overflow');
}
  $s['items'] = _append($s['items'], $item);
}
function pop(&$s) {
  if (is_empty($s)) {
  $panic('stack underflow');
}
  $n = _len($s['items']);
  $val = $s['items'][$n - 1];
  $s['items'] = array_slice($s['items'], 0, $n - 1 - 0);
  return $val;
}
function peek($s) {
  if (is_empty($s)) {
  $panic('peek from empty stack');
}
  return $s['items'][_len($s['items']) - 1];
}
function contains($s, $item) {
  $i = 0;
  while ($i < _len($s['items'])) {
  if ($s['items'][$i] == $item) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function stack_repr($s) {
  return _str($s['items']);
}
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
  echo rtrim(_str(contains($s, 1))), PHP_EOL;
  echo rtrim(_str(contains($s, 9))), PHP_EOL;
}
main();
