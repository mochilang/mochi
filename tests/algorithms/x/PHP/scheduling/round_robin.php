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
function _intdiv($a, $b) {
    if ($b === 0 || $b === '0') {
        throw new DivisionByZeroError();
    }
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
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
function calculate_waiting_times($burst_times) {
  $quantum = 2;
  $rem = [];
  $i = 0;
  while ($i < count($burst_times)) {
  $rem = _append($rem, $burst_times[$i]);
  $i = _iadd($i, 1);
};
  $waiting = [];
  $i = 0;
  while ($i < count($burst_times)) {
  $waiting = _append($waiting, 0);
  $i = _iadd($i, 1);
};
  $t = 0;
  while (true) {
  $done = true;
  $j = 0;
  while ($j < count($burst_times)) {
  if ($rem[$j] > 0) {
  $done = false;
  if ($rem[$j] > $quantum) {
  $t = _iadd($t, $quantum);
  $rem[$j] = _isub($rem[$j], $quantum);
} else {
  $t = _iadd($t, $rem[$j]);
  $waiting[$j] = _isub($t, $burst_times[$j]);
  $rem[$j] = 0;
};
}
  $j = _iadd($j, 1);
};
  if ($done) {
  return $waiting;
}
};
  return $waiting;
}
function calculate_turn_around_times($burst_times, $waiting_times) {
  $result = [];
  $i = 0;
  while ($i < count($burst_times)) {
  $result = _append($result, _iadd($burst_times[$i], $waiting_times[$i]));
  $i = _iadd($i, 1);
};
  return $result;
}
function mean($values) {
  $total = 0;
  $i = 0;
  while ($i < count($values)) {
  $total = _iadd($total, $values[$i]);
  $i = _iadd($i, 1);
};
  return (floatval($total)) / (floatval(count($values)));
}
function format_float_5($x) {
  $scaled = intval($x * 100000.0 + 0.5);
  $int_part = _intdiv($scaled, 100000);
  $frac_part = _imod($scaled, 100000);
  $frac_str = _str($frac_part);
  while (strlen($frac_str) < 5) {
  $frac_str = '0' . $frac_str;
};
  return _str($int_part) . '.' . $frac_str;
}
function main() {
  $burst_times = [3, 5, 7];
  $waiting_times = calculate_waiting_times($burst_times);
  $turn_around_times = calculate_turn_around_times($burst_times, $waiting_times);
  echo rtrim('Process ID 	Burst Time 	Waiting Time 	Turnaround Time'), PHP_EOL;
  $i = 0;
  while ($i < count($burst_times)) {
  $line = '  ' . _str(_iadd($i, 1)) . '		  ' . _str($burst_times[$i]) . '		  ' . _str($waiting_times[$i]) . '		  ' . _str($turn_around_times[$i]);
  echo rtrim($line), PHP_EOL;
  $i = _iadd($i, 1);
};
  echo rtrim(''), PHP_EOL;
  echo rtrim('Average waiting time = ' . format_float_5(mean($waiting_times))), PHP_EOL;
  echo rtrim('Average turn around time = ' . format_float_5(mean($turn_around_times))), PHP_EOL;
}
main();
