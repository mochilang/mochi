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
function sort_jobs_by_profit($jobs) {
  global $jobs1, $jobs2;
  $js = $jobs;
  $i = 0;
  while ($i < count($js)) {
  $j = 0;
  while ($j < _isub(_isub(count($js), $i), 1)) {
  $a = $js[$j];
  $b = $js[_iadd($j, 1)];
  if ($a['profit'] < $b['profit']) {
  $js[$j] = $b;
  $js[_iadd($j, 1)] = $a;
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $js;
}
function max_deadline($jobs) {
  global $jobs1, $jobs2;
  $max_d = 0;
  $i = 0;
  while ($i < count($jobs)) {
  $job = $jobs[$i];
  $d = $job['deadline'];
  if ($d > $max_d) {
  $max_d = $d;
}
  $i = _iadd($i, 1);
};
  return $max_d;
}
function job_sequencing_with_deadlines($jobs) {
  global $jobs1, $jobs2;
  $js = sort_jobs_by_profit($jobs);
  $max_d = max_deadline($js);
  $time_slots = [];
  $t = 0;
  while ($t < $max_d) {
  $time_slots = _append($time_slots, _isub(0, 1));
  $t = _iadd($t, 1);
};
  $count = 0;
  $max_profit = 0;
  $i = 0;
  while ($i < count($js)) {
  $job = $js[$i];
  $j = _isub($job['deadline'], 1);
  while ($j >= 0) {
  if ($time_slots[$j] == _isub(0, 1)) {
  $time_slots[$j] = $job['id'];
  $count = _iadd($count, 1);
  $max_profit = _iadd($max_profit, $job['profit']);
  break;
}
  $j = _isub($j, 1);
};
  $i = _iadd($i, 1);
};
  $result = [];
  $result = _append($result, $count);
  $result = _append($result, $max_profit);
  return $result;
}
$jobs1 = [];
$jobs1 = _append($jobs1, ['id' => 1, 'deadline' => 4, 'profit' => 20]);
$jobs1 = _append($jobs1, ['id' => 2, 'deadline' => 1, 'profit' => 10]);
$jobs1 = _append($jobs1, ['id' => 3, 'deadline' => 1, 'profit' => 40]);
$jobs1 = _append($jobs1, ['id' => 4, 'deadline' => 1, 'profit' => 30]);
echo rtrim(_str(job_sequencing_with_deadlines($jobs1))), PHP_EOL;
$jobs2 = [];
$jobs2 = _append($jobs2, ['id' => 1, 'deadline' => 2, 'profit' => 100]);
$jobs2 = _append($jobs2, ['id' => 2, 'deadline' => 1, 'profit' => 19]);
$jobs2 = _append($jobs2, ['id' => 3, 'deadline' => 2, 'profit' => 27]);
$jobs2 = _append($jobs2, ['id' => 4, 'deadline' => 1, 'profit' => 25]);
$jobs2 = _append($jobs2, ['id' => 5, 'deadline' => 1, 'profit' => 15]);
echo rtrim(_str(job_sequencing_with_deadlines($jobs2))), PHP_EOL;
