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
  global $q, $r1, $r2, $r3, $r4;
  return ['stack1' => $items, 'stack2' => []];
}
function len_queue($q) {
  global $r1, $r2, $r3, $r4;
  return _len($q['stack1']) + _len($q['stack2']);
}
function str_queue($q) {
  global $r1, $r2, $r3, $r4;
  $items = [];
  $i = _len($q['stack2']) - 1;
  while ($i >= 0) {
  $items = _append($items, $q['stack2'][$i]);
  $i = $i - 1;
};
  $j = 0;
  while ($j < _len($q['stack1'])) {
  $items = _append($items, $q['stack1'][$j]);
  $j = $j + 1;
};
  $s = 'Queue((';
  $k = 0;
  while ($k < count($items)) {
  $s = $s . _str($items[$k]);
  if ($k < count($items) - 1) {
  $s = $s . ', ';
}
  $k = $k + 1;
};
  $s = $s . '))';
  return $s;
}
function put($q, $item) {
  global $r1, $r2, $r3, $r4;
  $s1 = $q['stack1'];
  $s1 = _append($s1, $item);
  return ['stack1' => $s1, 'stack2' => $q['stack2']];
}
function get($q) {
  global $r1, $r2, $r3, $r4;
  $s1 = $q['stack1'];
  $s2 = $q['stack2'];
  if (count($s2) == 0) {
  while (count($s1) > 0) {
  $idx = count($s1) - 1;
  $v = $s1[$idx];
  $new_s1 = [];
  $i = 0;
  while ($i < $idx) {
  $new_s1 = _append($new_s1, $s1[$i]);
  $i = $i + 1;
};
  $s1 = $new_s1;
  $s2 = _append($s2, $v);
};
}
  if (count($s2) == 0) {
  $panic('Queue is empty');
}
  $idx2 = count($s2) - 1;
  $value = $s2[$idx2];
  $new_s2 = [];
  $j = 0;
  while ($j < $idx2) {
  $new_s2 = _append($new_s2, $s2[$j]);
  $j = $j + 1;
};
  $s2 = $new_s2;
  return ['queue' => ['stack1' => $s1, 'stack2' => $s2], 'value' => $value];
}
$q = new_queue([10, 20, 30]);
$r1 = get($q);
$q = $r1['queue'];
echo rtrim(json_encode($r1['value'], 1344)), PHP_EOL;
$q = put($q, 40);
$r2 = get($q);
$q = $r2['queue'];
echo rtrim(json_encode($r2['value'], 1344)), PHP_EOL;
$r3 = get($q);
$q = $r3['queue'];
echo rtrim(json_encode($r3['value'], 1344)), PHP_EOL;
echo rtrim(json_encode(len_queue($q), 1344)), PHP_EOL;
$r4 = get($q);
$q = $r4['queue'];
echo rtrim(json_encode($r4['value'], 1344)), PHP_EOL;
