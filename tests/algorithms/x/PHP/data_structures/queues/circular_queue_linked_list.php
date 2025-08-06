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
function create_queue($capacity) {
  $data = [];
  $next = [];
  $prev = [];
  $i = 0;
  while ($i < $capacity) {
  $data = _append($data, '');
  $next = _append($next, ($i + 1) % $capacity);
  $prev = _append($prev, ($i - 1 + $capacity) % $capacity);
  $i = $i + 1;
};
  return ['data' => $data, 'next' => $next, 'prev' => $prev, 'front' => 0, 'rear' => 0];
}
function is_empty($q) {
  return $q['front'] == $q['rear'] && $q['data'][$q['front']] == '';
}
function check_can_perform($q) {
  if (is_empty($q)) {
  $panic('Empty Queue');
}
}
function check_is_full($q) {
  if ($q['next'][$q['rear']] == $q['front']) {
  $panic('Full Queue');
}
}
function peek($q) {
  check_can_perform($q);
  return $q['data'][$q['front']];
}
function enqueue(&$q, $value) {
  check_is_full($q);
  if (!is_empty($q)) {
  $q['rear'] = $q['next'][$q['rear']];
}
  $data = $q['data'];
  $data[$q['rear']] = $value;
  $q['data'] = $data;
  return $q;
}
function dequeue(&$q) {
  check_can_perform($q);
  $data = $q['data'];
  $val = $data[$q['front']];
  $data[$q['front']] = '';
  $q['data'] = $data;
  if ($q['front'] != $q['rear']) {
  $q['front'] = $q['next'][$q['front']];
}
  return ['queue' => $q, 'value' => $val];
}
function main() {
  $q = create_queue(3);
  echo rtrim(_str(is_empty($q))), PHP_EOL;
  $q = enqueue($q, 'a');
  $q = enqueue($q, 'b');
  echo rtrim(peek($q)), PHP_EOL;
  $res = dequeue($q);
  $q = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = dequeue($q);
  $q = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(_str(is_empty($q))), PHP_EOL;
}
main();
