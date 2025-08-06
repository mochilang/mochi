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
function new_queue($items) {
  global $q, $res, $front;
  return ['entries' => $items];
}
function len_queue($q) {
  global $res, $front;
  return _len($q['entries']);
}
function str_queue($q) {
  global $res, $front;
  $s = 'Queue((';
  $i = 0;
  while ($i < _len($q['entries'])) {
  $s = $s . _str($q['entries'][$i]);
  if ($i < _len($q['entries']) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . '))';
  return $s;
}
function put($q, $item) {
  global $res, $front;
  $e = $q['entries'];
  $e = _append($e, $item);
  return ['entries' => $e];
}
function get($q) {
  global $res, $front;
  if (_len($q['entries']) == 0) {
  $panic('Queue is empty');
}
  $value = $q['entries'][0];
  $new_entries = [];
  $i = 1;
  while ($i < _len($q['entries'])) {
  $new_entries = _append($new_entries, $q['entries'][$i]);
  $i = $i + 1;
};
  return ['queue' => ['entries' => $new_entries], 'value' => $value];
}
function rotate($q, $rotation) {
  global $res, $front;
  $e = $q['entries'];
  $r = 0;
  while ($r < $rotation) {
  if (count($e) > 0) {
  $first = $e[0];
  $rest = [];
  $i = 1;
  while ($i < count($e)) {
  $rest = _append($rest, $e[$i]);
  $i = $i + 1;
};
  $rest = _append($rest, $first);
  $e = $rest;
}
  $r = $r + 1;
};
  return ['entries' => $e];
}
function get_front($q) {
  global $res, $front;
  return $q['entries'][0];
}
$q = new_queue([]);
echo rtrim(json_encode(len_queue($q), 1344)), PHP_EOL;
$q = put($q, 10);
$q = put($q, 20);
$q = put($q, 30);
$q = put($q, 40);
echo rtrim(str_queue($q)), PHP_EOL;
$res = get($q);
$q = $res['queue'];
echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
echo rtrim(str_queue($q)), PHP_EOL;
$q = rotate($q, 2);
echo rtrim(str_queue($q)), PHP_EOL;
$front = get_front($q);
echo rtrim(json_encode($front, 1344)), PHP_EOL;
echo rtrim(str_queue($q)), PHP_EOL;
