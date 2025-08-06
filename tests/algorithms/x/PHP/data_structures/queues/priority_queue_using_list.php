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
function panic($msg) {
  echo rtrim($msg), PHP_EOL;
}
function fpq_new() {
  return ['queues' => [[], [], []]];
}
function fpq_enqueue(&$fpq, $priority, $data) {
  if ($priority < 0 || $priority >= _len($fpq['queues'])) {
  panic('Valid priorities are 0, 1, and 2');
  return $fpq;
}
  if (_len($fpq['queues'][$priority]) >= 100) {
  panic('Maximum queue size is 100');
  return $fpq;
}
  $qs = $fpq['queues'];
  $qs[$priority] = _append($qs[$priority], $data);
  $fpq['queues'] = $qs;
  return $fpq;
}
function fpq_dequeue(&$fpq) {
  $qs = $fpq['queues'];
  $i = 0;
  while ($i < count($qs)) {
  $q = $qs[$i];
  if (count($q) > 0) {
  $val = $q[0];
  $new_q = [];
  $j = 1;
  while ($j < count($q)) {
  $new_q = _append($new_q, $q[$j]);
  $j = $j + 1;
};
  $qs[$i] = $new_q;
  $fpq['queues'] = $qs;
  return ['queue' => $fpq, 'value' => $val];
}
  $i = $i + 1;
};
  panic('All queues are empty');
  return ['queue' => $fpq, 'value' => 0];
}
function fpq_to_string($fpq) {
  $lines = [];
  $i = 0;
  while ($i < _len($fpq['queues'])) {
  $q_str = '[';
  $q = $fpq['queues'][$i];
  $j = 0;
  while ($j < count($q)) {
  if ($j > 0) {
  $q_str = $q_str . ', ';
}
  $q_str = $q_str . _str($q[$j]);
  $j = $j + 1;
};
  $q_str = $q_str . ']';
  $lines = _append($lines, 'Priority ' . _str($i) . ': ' . $q_str);
  $i = $i + 1;
};
  $res = '';
  $i = 0;
  while ($i < count($lines)) {
  if ($i > 0) {
  $res = $res . '
';
}
  $res = $res . $lines[$i];
  $i = $i + 1;
};
  return $res;
}
function epq_new() {
  return ['queue' => []];
}
function epq_enqueue(&$epq, $data) {
  if (_len($epq['queue']) >= 100) {
  panic('Maximum queue size is 100');
  return $epq;
}
  $epq['queue'] = _append($epq['queue'], $data);
  return $epq;
}
function epq_dequeue(&$epq) {
  if (_len($epq['queue']) == 0) {
  panic('The queue is empty');
  return ['queue' => $epq, 'value' => 0];
}
  $min_val = $epq['queue'][0];
  $idx = 0;
  $i = 1;
  while ($i < _len($epq['queue'])) {
  $v = $epq['queue'][$i];
  if ($v < $min_val) {
  $min_val = $v;
  $idx = $i;
}
  $i = $i + 1;
};
  $new_q = [];
  $i = 0;
  while ($i < _len($epq['queue'])) {
  if ($i != $idx) {
  $new_q = _append($new_q, $epq['queue'][$i]);
}
  $i = $i + 1;
};
  $epq['queue'] = $new_q;
  return ['queue' => $epq, 'value' => $min_val];
}
function epq_to_string($epq) {
  return _str($epq['queue']);
}
function fixed_priority_queue() {
  $fpq = fpq_new();
  $fpq = fpq_enqueue($fpq, 0, 10);
  $fpq = fpq_enqueue($fpq, 1, 70);
  $fpq = fpq_enqueue($fpq, 0, 100);
  $fpq = fpq_enqueue($fpq, 2, 1);
  $fpq = fpq_enqueue($fpq, 2, 5);
  $fpq = fpq_enqueue($fpq, 1, 7);
  $fpq = fpq_enqueue($fpq, 2, 4);
  $fpq = fpq_enqueue($fpq, 1, 64);
  $fpq = fpq_enqueue($fpq, 0, 128);
  echo rtrim(fpq_to_string($fpq)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(fpq_to_string($fpq)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = fpq_dequeue($fpq);
  $fpq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
}
function element_priority_queue() {
  $epq = epq_new();
  $epq = epq_enqueue($epq, 10);
  $epq = epq_enqueue($epq, 70);
  $epq = epq_enqueue($epq, 100);
  $epq = epq_enqueue($epq, 1);
  $epq = epq_enqueue($epq, 5);
  $epq = epq_enqueue($epq, 7);
  $epq = epq_enqueue($epq, 4);
  $epq = epq_enqueue($epq, 64);
  $epq = epq_enqueue($epq, 128);
  echo rtrim(epq_to_string($epq)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  echo rtrim(epq_to_string($epq)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
  $res = epq_dequeue($epq);
  $epq = $res['queue'];
  echo rtrim(json_encode($res['value'], 1344)), PHP_EOL;
}
function main() {
  fixed_priority_queue();
  element_priority_queue();
}
main();
