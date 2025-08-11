<?php
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
function _len($x) {
    if ($x === null) { return 0; }
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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function make_process($name, $arrival, $burst) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $mlfq, $finish_queue;
  return ['process_name' => $name, 'arrival_time' => $arrival, 'stop_time' => $arrival, 'burst_time' => $burst, 'waiting_time' => 0, 'turnaround_time' => 0];
};
  function make_mlfq($nqueues, $time_slices, $queue, $current_time) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $mlfq, $finish_queue;
  return ['number_of_queues' => $nqueues, 'time_slices' => $time_slices, 'ready_queue' => $queue, 'current_time' => $current_time, 'finish_queue' => []];
};
  function calculate_sequence_of_finish_queue($mlfq) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $finish_queue;
  $seq = [];
  $i = 0;
  while ($i < _len($mlfq['finish_queue'])) {
  $p = $mlfq['finish_queue'][$i];
  $seq = _append($seq, $p['process_name']);
  $i = _iadd($i, 1);
};
  return $seq;
};
  function calculate_waiting_time($queue) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $mlfq, $finish_queue;
  $times = [];
  $i = 0;
  while ($i < count($queue)) {
  $p = $queue[$i];
  $times = _append($times, $p['waiting_time']);
  $i = _iadd($i, 1);
};
  return $times;
};
  function calculate_turnaround_time($queue) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $mlfq, $finish_queue;
  $times = [];
  $i = 0;
  while ($i < count($queue)) {
  $p = $queue[$i];
  $times = _append($times, $p['turnaround_time']);
  $i = _iadd($i, 1);
};
  return $times;
};
  function calculate_completion_time($queue) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $mlfq, $finish_queue;
  $times = [];
  $i = 0;
  while ($i < count($queue)) {
  $p = $queue[$i];
  $times = _append($times, $p['stop_time']);
  $i = _iadd($i, 1);
};
  return $times;
};
  function calculate_remaining_burst_time_of_processes($queue) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $mlfq, $finish_queue;
  $times = [];
  $i = 0;
  while ($i < count($queue)) {
  $p = $queue[$i];
  $times = _append($times, $p['burst_time']);
  $i = _iadd($i, 1);
};
  return $times;
};
  function update_waiting_time($mlfq, &$process) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $finish_queue;
  $process['waiting_time'] = $process['waiting_time'] + ($mlfq['current_time'] - $process['stop_time']);
  return $process['waiting_time'];
};
  function first_come_first_served(&$mlfq, $ready_queue) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $finish_queue;
  $finished = [];
  $rq = $ready_queue;
  while (count($rq) != 0) {
  $cp = $rq[0];
  $rq = array_slice($rq, 1, count($rq) - 1);
  if ($mlfq['current_time'] < $cp['arrival_time']) {
  $mlfq['current_time'] = $cp['arrival_time'];
}
  update_waiting_time($mlfq, $cp);
  $mlfq['current_time'] = $mlfq['current_time'] + $cp['burst_time'];
  $cp['burst_time'] = 0;
  $cp['turnaround_time'] = $mlfq['current_time'] - $cp['arrival_time'];
  $cp['stop_time'] = $mlfq['current_time'];
  $finished = _append($finished, $cp);
};
  $mlfq['finish_queue'] = array_merge($mlfq[$finish_queue], $finished);
  return $finished;
};
  function round_robin(&$mlfq, $ready_queue, $time_slice) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $finish_queue;
  $finished = [];
  $rq = $ready_queue;
  $count = count($rq);
  $i = 0;
  while ($i < $count) {
  $cp = $rq[0];
  $rq = array_slice($rq, 1, count($rq) - 1);
  if ($mlfq['current_time'] < $cp['arrival_time']) {
  $mlfq['current_time'] = $cp['arrival_time'];
}
  update_waiting_time($mlfq, $cp);
  if ($cp['burst_time'] > $time_slice) {
  $mlfq['current_time'] = _iadd($mlfq['current_time'], $time_slice);
  $cp['burst_time'] = _isub($cp['burst_time'], $time_slice);
  $cp['stop_time'] = $mlfq['current_time'];
  $rq = _append($rq, $cp);
} else {
  $mlfq['current_time'] = $mlfq['current_time'] + $cp['burst_time'];
  $cp['burst_time'] = 0;
  $cp['stop_time'] = $mlfq['current_time'];
  $cp['turnaround_time'] = $mlfq['current_time'] - $cp['arrival_time'];
  $finished = _append($finished, $cp);
}
  $i = _iadd($i, 1);
};
  $mlfq['finish_queue'] = array_merge($mlfq[$finish_queue], $finished);
  return ['finished' => $finished, 'ready' => $rq];
};
  function multi_level_feedback_queue(&$mlfq) {
  global $P1, $P2, $P3, $P4, $number_of_queues, $time_slices, $queue, $finish_queue;
  $i = 0;
  while ($i < _isub($mlfq['number_of_queues'], 1)) {
  $rr = round_robin($mlfq, $mlfq['ready_queue'], $mlfq['time_slices'][$i]);
  $mlfq['ready_queue'] = $rr['ready'];
  $i = _iadd($i, 1);
};
  first_come_first_served($mlfq, $mlfq['ready_queue']);
  return $mlfq['finish_queue'];
};
  $P1 = make_process('P1', 0, 53);
  $P2 = make_process('P2', 0, 17);
  $P3 = make_process('P3', 0, 68);
  $P4 = make_process('P4', 0, 24);
  $number_of_queues = 3;
  $time_slices = [17, 25];
  $queue = [$P1, $P2, $P3, $P4];
  $mlfq = make_mlfq($number_of_queues, $time_slices, $queue, 0);
  $finish_queue = multi_level_feedback_queue($mlfq);
  echo rtrim('waiting time:			' . _str(calculate_waiting_time([$P1, $P2, $P3, $P4]))), PHP_EOL;
  echo rtrim('completion time:		' . _str(calculate_completion_time([$P1, $P2, $P3, $P4]))), PHP_EOL;
  echo rtrim('turnaround time:		' . _str(calculate_turnaround_time([$P1, $P2, $P3, $P4]))), PHP_EOL;
  echo rtrim('sequence of finished processes:	' . _str(calculate_sequence_of_finish_queue($mlfq))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
