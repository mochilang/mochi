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
function empty_queue() {
  return ['stack' => [], 'length' => 0];
}
function put($q, $item) {
  $s = _append($q['stack'], $item);
  return ['stack' => $s, 'length' => $q['length'] + 1];
}
function drop_first($xs) {
  $res = [];
  $i = 1;
  while ($i < count($xs)) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function drop_last($xs) {
  $res = [];
  $i = 0;
  while ($i < count($xs) - 1) {
  $res = _append($res, $xs[$i]);
  $i = $i + 1;
};
  return $res;
}
function rotate($q, $rotation) {
  $s = $q['stack'];
  $i = 0;
  while ($i < $rotation && count($s) > 0) {
  $temp = $s[0];
  $s = drop_first($s);
  $s = _append($s, $temp);
  $i = $i + 1;
};
  return ['stack' => $s, 'length' => $q['length']];
}
function get($q) {
  if ($q['length'] == 0) {
  $panic('queue empty');
}
  $q1 = rotate($q, 1);
  $v = $q1['stack'][$q1['length'] - 1];
  $s = drop_last($q1['stack']);
  $q2 = ['stack' => $s, 'length' => $q1['length']];
  $q2 = rotate($q2, $q2['length'] - 1);
  $q2 = ['stack' => $q2['stack'], 'length' => $q2['length'] - 1];
  return ['queue' => $q2, 'value' => $v];
}
function front($q) {
  $r = get($q);
  $q2 = put($r['queue'], $r['value']);
  $q2 = rotate($q2, $q2['length'] - 1);
  return ['queue' => $q2, 'value' => $r['value']];
}
function size($q) {
  return $q['length'];
}
function to_string($q) {
  $s = '<';
  if ($q['length'] > 0) {
  $s = $s . _str($q['stack'][0]);
  $i = 1;
  while ($i < $q['length']) {
  $s = $s . ', ' . _str($q['stack'][$i]);
  $i = $i + 1;
};
}
  $s = $s . '>';
  return $s;
}
function main() {
  $q = empty_queue();
  $q = put($q, 1);
  $q = put($q, 2);
  $q = put($q, 3);
  echo rtrim(to_string($q)), PHP_EOL;
  $g = get($q);
  $q = $g['queue'];
  echo rtrim(json_encode($g['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($q)), PHP_EOL;
  $f = front($q);
  $q = $f['queue'];
  echo rtrim(json_encode($f['value'], 1344)), PHP_EOL;
  echo rtrim(to_string($q)), PHP_EOL;
  echo rtrim(json_encode(size($q), 1344)), PHP_EOL;
}
main();
