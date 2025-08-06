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
function to_string($list) {
  if (_len($list['data']) == 0) {
  return '';
}
  $s = _str($list['data'][0]);
  $i = 1;
  while ($i < _len($list['data'])) {
  $s = $s . ' -> ' . _str($list['data'][$i]);
  $i = $i + 1;
};
  return $s;
}
function reverse_k_nodes($list, $k) {
  if ($k <= 1) {
  return $list;
}
  $res = [];
  $i = 0;
  while ($i < _len($list['data'])) {
  $j = 0;
  $group = [];
  while ($j < $k && $i + $j < _len($list['data'])) {
  $group = _append($group, $list['data'][$i + $j]);
  $j = $j + 1;
};
  if (count($group) == $k) {
  $g = $k - 1;
  while ($g >= 0) {
  $res = _append($res, $group[$g]);
  $g = $g - 1;
};
} else {
  $g = 0;
  while ($g < count($group)) {
  $res = _append($res, $group[$g]);
  $g = $g + 1;
};
}
  $i = $i + $k;
};
  return ['data' => $res];
}
function main() {
  $ll = ['data' => [1, 2, 3, 4, 5]];
  echo rtrim('Original Linked List: ' . to_string($ll)), PHP_EOL;
  $k = 2;
  $ll = reverse_k_nodes($ll, $k);
  echo rtrim('After reversing groups of size ' . _str($k) . ': ' . to_string($ll)), PHP_EOL;
}
main();
