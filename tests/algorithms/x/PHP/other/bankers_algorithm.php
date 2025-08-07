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
  function processes_resource_summation($alloc) {
  global $claim_vector, $allocated_resources_table, $maximum_claim_table;
  $resources = count($alloc[0]);
  $sums = [];
  $i = 0;
  while ($i < $resources) {
  $total = 0;
  $j = 0;
  while ($j < count($alloc)) {
  $total = $total + $alloc[$j][$i];
  $j = $j + 1;
};
  $sums = _append($sums, $total);
  $i = $i + 1;
};
  return $sums;
};
  function available_resources($claim, $alloc_sum) {
  global $claim_vector, $allocated_resources_table, $maximum_claim_table;
  $avail = [];
  $i = 0;
  while ($i < count($claim)) {
  $avail = _append($avail, $claim[$i] - $alloc_sum[$i]);
  $i = $i + 1;
};
  return $avail;
};
  function need($max, $alloc) {
  global $claim_vector, $allocated_resources_table, $maximum_claim_table;
  $needs = [];
  $i = 0;
  while ($i < count($max)) {
  $row = [];
  $j = 0;
  while ($j < count($max[0])) {
  $row = _append($row, $max[$i][$j] - $alloc[$i][$j]);
  $j = $j + 1;
};
  $needs = _append($needs, $row);
  $i = $i + 1;
};
  return $needs;
};
  function pretty_print($claim, $alloc, $max) {
  global $claim_vector, $allocated_resources_table, $maximum_claim_table;
  echo rtrim('         Allocated Resource Table'), PHP_EOL;
  $i = 0;
  while ($i < count($alloc)) {
  $row = $alloc[$i];
  $line = 'P' . _str($i + 1) . '       ';
  $j = 0;
  while ($j < count($row)) {
  $line = $line . _str($row[$j]);
  if ($j < count($row) - 1) {
  $line = $line . '        ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('         System Resource Table'), PHP_EOL;
  $i = 0;
  while ($i < count($max)) {
  $row = $max[$i];
  $line = 'P' . _str($i + 1) . '       ';
  $j = 0;
  while ($j < count($row)) {
  $line = $line . _str($row[$j]);
  if ($j < count($row) - 1) {
  $line = $line . '        ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $i = $i + 1;
};
  $usage = '';
  $i = 0;
  while ($i < count($claim)) {
  if ($i > 0) {
  $usage = $usage . ' ';
}
  $usage = $usage . _str($claim[$i]);
  $i = $i + 1;
};
  $alloc_sum = processes_resource_summation($alloc);
  $avail = available_resources($claim, $alloc_sum);
  $avail_str = '';
  $i = 0;
  while ($i < count($avail)) {
  if ($i > 0) {
  $avail_str = $avail_str . ' ';
}
  $avail_str = $avail_str . _str($avail[$i]);
  $i = $i + 1;
};
  echo rtrim('Current Usage by Active Processes: ' . $usage), PHP_EOL;
  echo rtrim('Initial Available Resources:       ' . $avail_str), PHP_EOL;
};
  function bankers_algorithm($claim, $alloc, $max) {
  global $claim_vector, $allocated_resources_table, $maximum_claim_table;
  $need_list = need($max, $alloc);
  $alloc_sum = processes_resource_summation($alloc);
  $avail = available_resources($claim, $alloc_sum);
  echo rtrim('__________________________________________________'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $finished = [];
  $i = 0;
  while ($i < count($need_list)) {
  $finished = _append($finished, false);
  $i = $i + 1;
};
  $remaining = count($need_list);
  while ($remaining > 0) {
  $safe = false;
  $p = 0;
  while ($p < count($need_list)) {
  if (!$finished[$p]) {
  $exec = true;
  $r = 0;
  while ($r < count($avail)) {
  if ($need_list[$p][$r] > $avail[$r]) {
  $exec = false;
  break;
}
  $r = $r + 1;
};
  if ($exec) {
  $safe = true;
  echo rtrim('Process ' . _str($p + 1) . ' is executing.'), PHP_EOL;
  $r = 0;
  while ($r < count($avail)) {
  $avail[$r] = $avail[$r] + $alloc[$p][$r];
  $r = $r + 1;
};
  $avail_str = '';
  $r = 0;
  while ($r < count($avail)) {
  if ($r > 0) {
  $avail_str = $avail_str . ' ';
}
  $avail_str = $avail_str . _str($avail[$r]);
  $r = $r + 1;
};
  echo rtrim('Updated available resource stack for processes: ' . $avail_str), PHP_EOL;
  echo rtrim('The process is in a safe state.'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $finished[$p] = true;
  $remaining = $remaining - 1;
};
}
  $p = $p + 1;
};
  if (!$safe) {
  echo rtrim('System in unsafe state. Aborting...'), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  break;
}
};
};
  $claim_vector = [8, 5, 9, 7];
  $allocated_resources_table = [[2, 0, 1, 1], [0, 1, 2, 1], [4, 0, 0, 3], [0, 2, 1, 0], [1, 0, 3, 0]];
  $maximum_claim_table = [[3, 2, 1, 4], [0, 2, 5, 2], [5, 1, 0, 5], [1, 5, 3, 0], [3, 0, 3, 3]];
  pretty_print($claim_vector, $allocated_resources_table, $maximum_claim_table);
  bankers_algorithm($claim_vector, $allocated_resources_table, $maximum_claim_table);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
