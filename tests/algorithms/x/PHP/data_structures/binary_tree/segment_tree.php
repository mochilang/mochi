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
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv(intval($a), intval($b));
}
$__start_mem = memory_get_usage();
$__start = _now();
  $A = [];
  $N = 0;
  $st = [];
  function left_child($idx) {
  global $A, $N, $NEG_INF, $st;
  return $idx * 2;
};
  function right_child($idx) {
  global $A, $N, $NEG_INF, $st;
  return $idx * 2 + 1;
};
  function build($idx, $left, $right) {
  global $A, $N, $NEG_INF, $st;
  if ($left == $right) {
  $st[$idx] = $A[$left];
} else {
  $mid = _intdiv(($left + $right), 2);
  build(left_child($idx), $left, $mid);
  build(right_child($idx), $mid + 1, $right);
  $left_val = $st[left_child($idx)];
  $right_val = $st[right_child($idx)];
  $st[$idx] = ($left_val > $right_val ? $left_val : $right_val);
}
};
  function update_recursive($idx, $left, $right, $a, $b, $val) {
  global $A, $N, $NEG_INF, $st;
  if ($right < $a || $left > $b) {
  return true;
}
  if ($left == $right) {
  $st[$idx] = $val;
  return true;
}
  $mid = _intdiv(($left + $right), 2);
  update_recursive(left_child($idx), $left, $mid, $a, $b, $val);
  update_recursive(right_child($idx), $mid + 1, $right, $a, $b, $val);
  $left_val = $st[left_child($idx)];
  $right_val = $st[right_child($idx)];
  $st[$idx] = ($left_val > $right_val ? $left_val : $right_val);
  return true;
};
  function update($a, $b, $val) {
  global $A, $N, $NEG_INF, $st;
  return update_recursive(1, 0, $N - 1, $a - 1, $b - 1, $val);
};
  $NEG_INF = -1000000000;
  function query_recursive($idx, $left, $right, $a, $b) {
  global $A, $N, $NEG_INF, $st;
  if ($right < $a || $left > $b) {
  return $NEG_INF;
}
  if ($left >= $a && $right <= $b) {
  return $st[$idx];
}
  $mid = _intdiv(($left + $right), 2);
  $q1 = query_recursive(left_child($idx), $left, $mid, $a, $b);
  $q2 = query_recursive(right_child($idx), $mid + 1, $right, $a, $b);
  return ($q1 > $q2 ? $q1 : $q2);
};
  function query($a, $b) {
  global $A, $N, $NEG_INF, $st;
  return query_recursive(1, 0, $N - 1, $a - 1, $b - 1);
};
  function show_data() {
  global $A, $N, $NEG_INF, $st;
  $i = 0;
  $show_list = [];
  while ($i < $N) {
  $show_list = _append($show_list, query($i + 1, $i + 1));
  $i = $i + 1;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($show_list, 1344)))))), PHP_EOL;
};
  function main() {
  global $A, $N, $NEG_INF, $st;
  $A = [1, 2, -4, 7, 3, -5, 6, 11, -20, 9, 14, 15, 5, 2, -8];
  $N = count($A);
  $i = 0;
  while ($i < 4 * $N) {
  $st = _append($st, 0);
  $i = $i + 1;
};
  if ($N > 0) {
  build(1, 0, $N - 1);
}
  echo rtrim(json_encode(query(4, 6), 1344)), PHP_EOL;
  echo rtrim(json_encode(query(7, 11), 1344)), PHP_EOL;
  echo rtrim(json_encode(query(7, 12), 1344)), PHP_EOL;
  update(1, 3, 111);
  echo rtrim(json_encode(query(1, 15), 1344)), PHP_EOL;
  update(7, 8, 235);
  show_data();
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
