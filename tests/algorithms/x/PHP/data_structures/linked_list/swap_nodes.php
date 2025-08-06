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
function empty_list() {
  return ['data' => []];
}
function push($list, $value) {
  $res = [$value];
  $res = array_merge($res, $list['data']);
  return ['data' => $res];
}
function swap_nodes($list, $v1, $v2) {
  if ($v1 == $v2) {
  return $list;
}
  $idx1 = 0 - 1;
  $idx2 = 0 - 1;
  $i = 0;
  while ($i < _len($list['data'])) {
  if ($list['data'][$i] == $v1 && $idx1 == 0 - 1) {
  $idx1 = $i;
}
  if ($list['data'][$i] == $v2 && $idx2 == 0 - 1) {
  $idx2 = $i;
}
  $i = $i + 1;
};
  if ($idx1 == 0 - 1 || $idx2 == 0 - 1) {
  return $list;
}
  $res = $list['data'];
  $temp = $res[$idx1];
  $res[$idx1] = $res[$idx2];
  $res[$idx2] = $temp;
  return ['data' => $res];
}
function to_string($list) {
  return _str($list['data']);
}
function main() {
  $ll = empty_list();
  $i = 5;
  while ($i > 0) {
  $ll = push($ll, $i);
  $i = $i - 1;
};
  echo rtrim('Original Linked List: ' . to_string($ll)), PHP_EOL;
  $ll = swap_nodes($ll, 1, 4);
  echo rtrim('Modified Linked List: ' . to_string($ll)), PHP_EOL;
  echo rtrim('After swapping the nodes whose data is 1 and 4.'), PHP_EOL;
}
main();
