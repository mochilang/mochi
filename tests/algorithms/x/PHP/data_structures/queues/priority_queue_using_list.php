<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
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
$__start_mem = memory_get_usage();
$__start = _now();
  function mochi_panic($msg) {
  echo rtrim($msg), PHP_EOL;
};
  function fpq_new() {
  return ['queues' => [[], [], []]];
};
  function fpq_enqueue($fpq, $priority, $data) {
  if ($priority < 0 || $priority >= count($fpq['queues'])) {
  mochi_panic('Valid priorities are 0, 1, and 2');
  return $fpq;
}
  if (count($fpq['queues'][$priority]) >= 100) {
  mochi_panic('Maximum queue size is 100');
  return $fpq;
}
  $qs = $fpq['queues'];
  $qs[$priority] = _append($qs[$priority], $data);
  $fpq['queues'] = $qs;
  return $fpq;
};
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
  mochi_panic('All queues are empty');
  return ['queue' => $fpq, 'value' => 0];
};
  function fpq_to_string($fpq) {
  $lines = [];
  $i = 0;
  while ($i < count($fpq['queues'])) {
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
};
  function epq_new() {
  return ['queue' => []];
};
  function epq_enqueue($epq, $data) {
  if (count($epq['queue']) >= 100) {
  mochi_panic('Maximum queue size is 100');
  return $epq;
}
  $epq['queue'] = _append($epq['queue'], $data);
  return $epq;
};
  function epq_dequeue(&$epq) {
  if (count($epq['queue']) == 0) {
  mochi_panic('The queue is empty');
  return ['queue' => $epq, 'value' => 0];
}
  $min_val = $epq['queue'][0];
  $idx = 0;
  $i = 1;
  while ($i < count($epq['queue'])) {
  $v = $epq['queue'][$i];
  if ($v < $min_val) {
  $min_val = $v;
  $idx = $i;
}
  $i = $i + 1;
};
  $new_q = [];
  $i = 0;
  while ($i < count($epq['queue'])) {
  if ($i != $idx) {
  $new_q = _append($new_q, $epq['queue'][$i]);
}
  $i = $i + 1;
};
  $epq['queue'] = $new_q;
  return ['queue' => $epq, 'value' => $min_val];
};
  function epq_to_string($epq) {
  return _str($epq['queue']);
};
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
};
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
};
  function main() {
  fixed_priority_queue();
  element_priority_queue();
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
