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
  $remaining_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $remaining_time = _append($remaining_time, $burst_time[$i]);
  $i = $i + 1;
};
  $waiting_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $waiting_time = _append($waiting_time, 0);
  $i = $i + 1;
};
  $complete = 0;
  $increment_time = 0;
  $minm = 1000000000;
  $short = 0;
  $check = false;
  while ($complete != $no_of_processes) {
  $j = 0;
  while ($j < $no_of_processes) {
  if ($arrival_time[$j] <= $increment_time && $remaining_time[$j] > 0 && $remaining_time[$j] < $minm) {
  $minm = $remaining_time[$j];
  $short = $j;
  $check = true;
}
  $j = $j + 1;
};
  if (!$check) {
  $increment_time = $increment_time + 1;
  continue;
}
  $remaining_time[$short] = $remaining_time[$short] - 1;
  $minm = $remaining_time[$short];
  if ($minm == 0) {
  $minm = 1000000000;
}
  if ($remaining_time[$short] == 0) {
  $complete = $complete + 1;
  $check = false;
  $finish_time = $increment_time + 1;
  $finar = $finish_time - $arrival_time[$short];
  $waiting_time[$short] = $finar - $burst_time[$short];
  if ($waiting_time[$short] < 0) {
  $waiting_time[$short] = 0;
};
}
  $increment_time = $increment_time + 1;
};
  return $waiting_time;
};
  function calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time) {
  $turn_around_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $turn_around_time = _append($turn_around_time, $burst_time[$i] + $waiting_time[$i]);
  $i = $i + 1;
};
  return $turn_around_time;
};
  function mochi_to_float($x) {
  return $x * 1.0;
};
  function calculate_average_times($waiting_time, $turn_around_time, $no_of_processes) {
  $total_waiting_time = 0;
  $total_turn_around_time = 0;
  $i = 0;
  while ($i < $no_of_processes) {
  $total_waiting_time = $total_waiting_time + $waiting_time[$i];
  $total_turn_around_time = $total_turn_around_time + $turn_around_time[$i];
  $i = $i + 1;
};
  $avg_wait = floatval($total_waiting_time) / floatval($no_of_processes);
  $avg_turn = floatval($total_turn_around_time) / floatval($no_of_processes);
  echo rtrim('Average waiting time = ' . _str($avg_wait)), PHP_EOL;
  echo rtrim('Average turn around time = ' . _str($avg_turn)), PHP_EOL;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([1, 2, 3, 4], [3, 3, 5, 1], 4), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([1, 2, 3], [2, 5, 1], 3), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([2, 3], [5, 1], 2), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([3, 3, 5, 1], 4, [0, 3, 5, 0]), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([3, 3], 2, [0, 3]), 1344)))))), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([8, 10, 1], 3, [1, 0, 3]), 1344)))))), PHP_EOL;
  calculate_average_times([0, 3, 5, 0], [3, 6, 10, 1], 4);
  calculate_average_times([2, 3], [3, 6], 2);
  calculate_average_times([10, 4, 3], [2, 7, 6], 3);
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
