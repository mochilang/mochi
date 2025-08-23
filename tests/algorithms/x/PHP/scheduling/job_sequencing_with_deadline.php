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
  function sort_jobs_by_profit($jobs) {
  global $jobs1, $jobs2;
  $js = $jobs;
  $i = 0;
  while ($i < count($js)) {
  $j = 0;
  while ($j < count($js) - $i - 1) {
  $a = $js[$j];
  $b = $js[$j + 1];
  if ($a['profit'] < $b['profit']) {
  $js[$j] = $b;
  $js[$j + 1] = $a;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $js;
};
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
  $i = $i + 1;
};
  return $max_d;
};
  function job_sequencing_with_deadlines($jobs) {
  global $jobs1, $jobs2;
  $js = sort_jobs_by_profit($jobs);
  $max_d = max_deadline($js);
  $time_slots = [];
  $t = 0;
  while ($t < $max_d) {
  $time_slots = _append($time_slots, 0 - 1);
  $t = $t + 1;
};
  $count = 0;
  $max_profit = 0;
  $i = 0;
  while ($i < count($js)) {
  $job = $js[$i];
  $j = $job['deadline'] - 1;
  while ($j >= 0) {
  if ($time_slots[$j] == 0 - 1) {
  $time_slots[$j] = $job['id'];
  $count = $count + 1;
  $max_profit = $max_profit + $job['profit'];
  break;
}
  $j = $j - 1;
};
  $i = $i + 1;
};
  $result = [];
  $result = _append($result, $count);
  $result = _append($result, $max_profit);
  return $result;
};
  $jobs1 = [];
  $jobs1 = _append($jobs1, ['deadline' => 4, 'id' => 1, 'profit' => 20]);
  $jobs1 = _append($jobs1, ['deadline' => 1, 'id' => 2, 'profit' => 10]);
  $jobs1 = _append($jobs1, ['deadline' => 1, 'id' => 3, 'profit' => 40]);
  $jobs1 = _append($jobs1, ['deadline' => 1, 'id' => 4, 'profit' => 30]);
  echo rtrim(_str(job_sequencing_with_deadlines($jobs1))), PHP_EOL;
  $jobs2 = [];
  $jobs2 = _append($jobs2, ['deadline' => 2, 'id' => 1, 'profit' => 100]);
  $jobs2 = _append($jobs2, ['deadline' => 1, 'id' => 2, 'profit' => 19]);
  $jobs2 = _append($jobs2, ['deadline' => 2, 'id' => 3, 'profit' => 27]);
  $jobs2 = _append($jobs2, ['deadline' => 1, 'id' => 4, 'profit' => 25]);
  $jobs2 = _append($jobs2, ['deadline' => 1, 'id' => 5, 'profit' => 15]);
  echo rtrim(_str(job_sequencing_with_deadlines($jobs2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
