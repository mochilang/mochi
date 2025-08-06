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
function empty_list() {
  return ['data' => []];
}
function length($list) {
  return _len($list['data']);
}
function is_empty($list) {
  return _len($list['data']) == 0;
}
function to_string($list) {
  if (_len($list['data']) == 0) {
  return '';
}
  $s = _str($list['data'][0]);
  $i = 1;
  while ($i < _len($list['data'])) {
  $s = $s . '->' . _str($list['data'][$i]);
  $i = $i + 1;
};
  return $s;
}
function insert_nth($list, $index, $value) {
  if ($index < 0 || $index > _len($list['data'])) {
  $panic('index out of range');
}
  $res = [];
  $i = 0;
  while ($i < $index) {
  $res = _append($res, $list['data'][$i]);
  $i = $i + 1;
};
  $res = _append($res, $value);
  while ($i < _len($list['data'])) {
  $res = _append($res, $list['data'][$i]);
  $i = $i + 1;
};
  return ['data' => $res];
}
function insert_head($list, $value) {
  return insert_nth($list, 0, $value);
}
function insert_tail($list, $value) {
  return insert_nth($list, _len($list['data']), $value);
}
function delete_nth($list, $index) {
  if ($index < 0 || $index >= _len($list['data'])) {
  $panic('index out of range');
}
  $res = [];
  $i = 0;
  $removed = 0;
  while ($i < _len($list['data'])) {
  if ($i == $index) {
  $removed = $list['data'][$i];
} else {
  $res = _append($res, $list['data'][$i]);
}
  $i = $i + 1;
};
  return ['list' => ['data' => $res], 'value' => $removed];
}
function delete_head($list) {
  return delete_nth($list, 0);
}
function delete_tail($list) {
  return delete_nth($list, _len($list['data']) - 1);
}
function delete_value($list, $value) {
  $idx = 0;
  $found = false;
  while ($idx < _len($list['data'])) {
  if ($list['data'][$idx] == $value) {
  $found = true;
  break;
}
  $idx = $idx + 1;
};
  if (!$found) {
  $panic('value not found');
}
  return delete_nth($list, $idx);
}
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
}
main();
