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
  function josephus_recursive($num_people, $step_size) {
  global $r;
  if ($num_people <= 0 || $step_size <= 0) {
  $panic('num_people or step_size is not a positive integer.');
}
  if ($num_people == 1) {
  return 0;
}
  return fmod((josephus_recursive($num_people - 1, $step_size) + $step_size), $num_people);
};
  function find_winner($num_people, $step_size) {
  global $r;
  return josephus_recursive($num_people, $step_size) + 1;
};
  function remove_at($xs, $idx) {
  global $r;
  $res = [];
  $i = 0;
  while ($i < count($xs)) {
  if ($i != $idx) {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  function josephus_iterative($num_people, $step_size) {
  global $r;
  if ($num_people <= 0 || $step_size <= 0) {
  $panic('num_people or step_size is not a positive integer.');
}
  $circle = [];
  $i = 1;
  while ($i <= $num_people) {
  $circle = _append($circle, $i);
  $i = $i + 1;
};
  $current = 0;
  while (count($circle) > 1) {
  $current = fmod(($current + $step_size - 1), count($circle));
  $circle = remove_at($circle, $current);
};
  return $circle[0];
};
  $r = josephus_recursive(7, 3);
  echo rtrim(_str($r)), PHP_EOL;
  echo rtrim(_str(find_winner(7, 3))), PHP_EOL;
  echo rtrim(_str(josephus_iterative(7, 3))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
