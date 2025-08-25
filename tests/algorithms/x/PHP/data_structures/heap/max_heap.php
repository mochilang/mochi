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
        $q = bcdiv($sa, $sb, 0);
        $rem = bcmod($sa, $sb);
        $neg = ((strpos($sa, '-') === 0) xor (strpos($sb, '-') === 0));
        if ($neg && bccomp($rem, '0') != 0) {
            $q = bcsub($q, '1');
        }
        return intval($q);
    }
    $ai = intval($a);
    $bi = intval($b);
    $q = intdiv($ai, $bi);
    if ((($ai ^ $bi) < 0) && ($ai % $bi != 0)) {
        $q -= 1;
    }
    return $q;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $heap = [0];
  $size = 0;
  function swap_up($i) {
  global $heap, $size;
  $temp = $heap[$i];
  $idx = $i;
  while (_intdiv($idx, 2) > 0) {
  if ($heap[$idx] > $heap[_intdiv($idx, 2)]) {
  $heap[$idx] = $heap[_intdiv($idx, 2)];
  $heap[_intdiv($idx, 2)] = $temp;
}
  $idx = _intdiv($idx, 2);
};
};
  function insert($value) {
  global $heap, $size;
  $heap = _append($heap, $value);
  $size = $size + 1;
  swap_up($size);
};
  function swap_down($i) {
  global $heap, $size;
  $idx = $i;
  while ($size >= 2 * $idx) {
  $bigger_child = (2 * $idx + 1 > $size ? 2 * $idx : ($heap[2 * $idx] > $heap[2 * $idx + 1] ? 2 * $idx : 2 * $idx + 1));
  $temp = $heap[$idx];
  if ($heap[$idx] < $heap[$bigger_child]) {
  $heap[$idx] = $heap[$bigger_child];
  $heap[$bigger_child] = $temp;
}
  $idx = $bigger_child;
};
};
  function shrink() {
  global $heap, $size;
  $new_heap = [];
  $i = 0;
  while ($i <= $size) {
  $new_heap = _append($new_heap, $heap[$i]);
  $i = $i + 1;
};
  $heap = $new_heap;
};
  function pop() {
  global $heap, $size;
  $max_value = $heap[1];
  $heap[1] = $heap[$size];
  $size = $size - 1;
  shrink();
  swap_down(1);
  return $max_value;
};
  function get_list() {
  global $heap, $size;
  $out = [];
  $i = 1;
  while ($i <= $size) {
  $out = _append($out, $heap[$i]);
  $i = $i + 1;
};
  return $out;
};
  function mochi_len() {
  global $heap, $size;
  return $size;
};
  insert(6);
  insert(10);
  insert(15);
  insert(12);
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
  echo rtrim(json_encode(pop(), 1344)), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(get_list(), 1344)))))), PHP_EOL;
  echo rtrim(json_encode(mochi_len(), 1344)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
