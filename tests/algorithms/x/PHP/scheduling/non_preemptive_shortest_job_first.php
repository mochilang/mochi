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
  function calculate_waitingtime($arrival_time, $burst_time, $no_of_processes) {
  global $avg_turn, $avg_wait, $pid, $turn_around_time;
  $waiting_time = null;
  $remaining_time = null;
  $i = 0;
  while ($i < $no_of_processes) {
  $waiting_time = _append($waiting_time, 0);
  $remaining_time = _append($remaining_time, $burst_time[$i]);
  $i = $i + 1;
};
  $completed = 0;
  $total_time = 0;
  while ($completed != $no_of_processes) {
  $ready_process = [];
  $target_process = -1;
  $j = 0;
  while ($j < $no_of_processes) {
  if ($arrival_time[$j] <= $total_time && $remaining_time[$j] > 0) {
  $ready_process = _append($ready_process, $j);
}
  $j = $j + 1;
};
  if (count($ready_process) > 0) {
  $target_process = $ready_process[0];
  $k = 0;
  while ($k < count($ready_process)) {
  $idx = $ready_process[$k];
  if ($remaining_time[$idx] < $remaining_time[$target_process]) {
  $target_process = $idx;
}
  $k = $k + 1;
};
  $total_time = $total_time + $burst_time[$target_process];
  $completed = $completed + 1;
  $remaining_time[$target_process] = 0;
  $waiting_time[$target_process] = $total_time - $arrival_time[$target_process] - $burst_time[$target_process];
} else {
  $total_time = $total_time + 1;
}
};
  return $waiting_time;
};
  function calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time) {
  global $arrival_time, $avg_turn, $avg_wait, $pid;
  $turn_around_time = null;
  $i = 0;
  while ($i < $no_of_processes) {
  $turn_around_time = _append($turn_around_time, $burst_time[$i] + $waiting_time[$i]);
  $i = $i + 1;
};
  return $turn_around_time;
};
  function average($values) {
  global $arrival_time, $avg_turn, $avg_wait, $burst_time, $no_of_processes, $pid, $turn_around_time, $waiting_time;
  $total = 0;
  $i = 0;
  while ($i < count($values)) {
  $total = $total + $values[$i];
  $i = $i + 1;
};
  return (floatval($total)) / (floatval(count($values)));
};
  echo rtrim('[TEST CASE 01]'), PHP_EOL;
  $no_of_processes = 4;
  $burst_time = [2, 5, 3, 7];
  $arrival_time = [0, 0, 0, 0];
  $waiting_time = calculate_waitingtime($arrival_time, $burst_time, $no_of_processes);
  $turn_around_time = calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time);
  echo rtrim('PID	Burst Time	Arrival Time	Waiting Time	Turnaround Time'), PHP_EOL;
  $i = 0;
  while ($i < $no_of_processes) {
  $pid = $i + 1;
  echo rtrim(_str($pid) . '	' . _str($burst_time[$i]) . '			' . _str($arrival_time[$i]) . '				' . _str($waiting_time[$i]) . '				' . _str($turn_around_time[$i])), PHP_EOL;
  $i = $i + 1;
}
  $avg_wait = average($waiting_time);
  $avg_turn = average($turn_around_time);
  echo rtrim('
Average waiting time = ' . _str($avg_wait)), PHP_EOL;
  echo rtrim('Average turnaround time = ' . _str($avg_turn)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
