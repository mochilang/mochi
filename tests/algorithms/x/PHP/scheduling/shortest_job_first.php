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
  $remaining_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $remaining_time = _append($remaining_time, $burst_time[$i]);
  $i = _iadd($i, 1);
};
  $waiting_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $waiting_time = _append($waiting_time, 0);
  $i = _iadd($i, 1);
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
  $j = _iadd($j, 1);
};
  if (!$check) {
  $increment_time = _iadd($increment_time, 1);
  continue;
}
  $remaining_time[$short] = _isub($remaining_time[$short], 1);
  $minm = $remaining_time[$short];
  if ($minm == 0) {
  $minm = 1000000000;
}
  if ($remaining_time[$short] == 0) {
  $complete = _iadd($complete, 1);
  $check = false;
  $finish_time = _iadd($increment_time, 1);
  $finar = _isub($finish_time, $arrival_time[$short]);
  $waiting_time[$short] = _isub($finar, $burst_time[$short]);
  if ($waiting_time[$short] < 0) {
  $waiting_time[$short] = 0;
};
}
  $increment_time = _iadd($increment_time, 1);
};
  return $waiting_time;
}
function calculate_turnaroundtime($burst_time, $no_of_processes, $waiting_time) {
  $turn_around_time = [];
  $i = 0;
  while ($i < $no_of_processes) {
  $turn_around_time = _append($turn_around_time, _iadd($burst_time[$i], $waiting_time[$i]));
  $i = _iadd($i, 1);
};
  return $turn_around_time;
}
function to_float($x) {
  return _imul($x, 1.0);
}
function calculate_average_times($waiting_time, $turn_around_time, $no_of_processes) {
  $total_waiting_time = 0;
  $total_turn_around_time = 0;
  $i = 0;
  while ($i < $no_of_processes) {
  $total_waiting_time = _iadd($total_waiting_time, $waiting_time[$i]);
  $total_turn_around_time = _iadd($total_turn_around_time, $turn_around_time[$i]);
  $i = _iadd($i, 1);
};
  $avg_wait = floatval($total_waiting_time) / floatval($no_of_processes);
  $avg_turn = floatval($total_turn_around_time) / floatval($no_of_processes);
  echo rtrim('Average waiting time = ' . _str($avg_wait)), PHP_EOL;
  echo rtrim('Average turn around time = ' . _str($avg_turn)), PHP_EOL;
}
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([1, 2, 3, 4], [3, 3, 5, 1], 4), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([1, 2, 3], [2, 5, 1], 3), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_waitingtime([2, 3], [5, 1], 2), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([3, 3, 5, 1], 4, [0, 3, 5, 0]), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([3, 3], 2, [0, 3]), 1344)))))), PHP_EOL;
echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(calculate_turnaroundtime([8, 10, 1], 3, [1, 0, 3]), 1344)))))), PHP_EOL;
calculate_average_times([0, 3, 5, 0], [3, 6, 10, 1], 4);
calculate_average_times([2, 3], [3, 6], 2);
calculate_average_times([10, 4, 3], [2, 7, 6], 3);
