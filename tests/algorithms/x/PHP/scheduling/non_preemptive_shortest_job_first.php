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
function calculate_waitingtime($arrival_time, $burst_time, $no_of_processes) {
  global $turn_around_time, $pid, $avg_wait, $avg_turn;
  $waiting_time = null;
  $remaining_time = null;
  $i = 0;
  while ($i < $no_of_processes) {
  $waiting_time = _append($waiting_time, 0);
  $remaining_time = _append($remaining_time, $burst_time[$i]);
  $i = _iadd($i, 1);
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
  $j = _iadd($j, 1);
};
  if (count($ready_process) > 0) {
  $target_process = $ready_process[0];
  $k = 0;
  while ($k < count($ready_process)) {
  $idx = $ready_process[$k];
  if ($remaining_time[$idx] < $remaining_time[$target_process]) {
  $target_process = $idx;
}
  $k = _iadd($k, 1);
};
  $total_time = _iadd($total_time, $burst_time[$target_process]);
  $completed = _iadd($completed, 1);
  $remaining_time[$target_process] = 0;
  $waiting_time[$target_process] = _isub(_isub($total_time, $arrival_time[$target_process]), $burst_time[$target_process]);
} else {
  $total_time = _iadd($total_time, 1);
}
};
  return $waiting_time;
}
function calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time) {
  global $arrival_time, $pid, $avg_wait, $avg_turn;
  $turn_around_time = null;
  $i = 0;
  while ($i < $no_of_processes) {
  $turn_around_time = _append($turn_around_time, _iadd($burst_time[$i], $waiting_time[$i]));
  $i = _iadd($i, 1);
};
  return $turn_around_time;
}
function average($values) {
  global $no_of_processes, $burst_time, $arrival_time, $waiting_time, $turn_around_time, $pid, $avg_wait, $avg_turn;
  $total = 0;
  $i = 0;
  while ($i < count($values)) {
  $total = _iadd($total, $values[$i]);
  $i = _iadd($i, 1);
};
  return (floatval($total)) / (floatval(count($values)));
}
echo rtrim('[TEST CASE 01]'), PHP_EOL;
$no_of_processes = 4;
$burst_time = [2, 5, 3, 7];
$arrival_time = [0, 0, 0, 0];
$waiting_time = calculate_waitingtime($arrival_time, $burst_time, $no_of_processes);
$turn_around_time = calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time);
echo rtrim('PID	Burst Time	Arrival Time	Waiting Time	Turnaround Time'), PHP_EOL;
$i = 0;
while ($i < $no_of_processes) {
  $pid = _iadd($i, 1);
  echo rtrim(_str($pid) . '	' . _str($burst_time[$i]) . '			' . _str($arrival_time[$i]) . '				' . _str($waiting_time[$i]) . '				' . _str($turn_around_time[$i])), PHP_EOL;
  $i = _iadd($i, 1);
}
$avg_wait = average($waiting_time);
$avg_turn = average($turn_around_time);
echo rtrim('
Average waiting time = ' . _str($avg_wait)), PHP_EOL;
echo rtrim('Average turnaround time = ' . _str($avg_turn)), PHP_EOL;
