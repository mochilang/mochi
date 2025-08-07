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
  function mochi_floor($x) {
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function pow10($n) {
  $p = 1.0;
  $i = 0;
  while ($i < $n) {
  $p = $p * 10.0;
  $i = $i + 1;
};
  return $p;
};
  function mochi_round($x, $n) {
  $m = pow10($n);
  return mochi_floor($x * $m + 0.5) / $m;
};
  function clone_matrix($mat) {
  $new_mat = [];
  $i = 0;
  while ($i < count($mat)) {
  $row = [];
  $j = 0;
  while ($j < count($mat[$i])) {
  $row = _append($row, $mat[$i][$j]);
  $j = $j + 1;
};
  $new_mat = _append($new_mat, $row);
  $i = $i + 1;
};
  return $new_mat;
};
  function solve_simultaneous($equations) {
  $n = count($equations);
  if ($n == 0) {
  $panic('solve_simultaneous() requires n lists of length n+1');
}
  $m = $n + 1;
  $i = 0;
  while ($i < $n) {
  if (count($equations[$i]) != $m) {
  $panic('solve_simultaneous() requires n lists of length n+1');
}
  $i = $i + 1;
};
  $a = clone_matrix($equations);
  $row = 0;
  while ($row < $n) {
  $pivot = $row;
  while ($pivot < $n && $a[$pivot][$row] == 0.0) {
  $pivot = $pivot + 1;
};
  if ($pivot == $n) {
  $panic('solve_simultaneous() requires at least 1 full equation');
}
  if ($pivot != $row) {
  $temp = $a[$row];
  $a[$row] = $a[$pivot];
  $a[$pivot] = $temp;
}
  $pivot_val = $a[$row][$row];
  $col = 0;
  while ($col < $m) {
  $a[$row][$col] = $a[$row][$col] / $pivot_val;
  $col = $col + 1;
};
  $r = 0;
  while ($r < $n) {
  if ($r != $row) {
  $factor = $a[$r][$row];
  $c = 0;
  while ($c < $m) {
  $a[$r][$c] = $a[$r][$c] - $factor * $a[$row][$c];
  $c = $c + 1;
};
}
  $r = $r + 1;
};
  $row = $row + 1;
};
  $res = [];
  $k = 0;
  while ($k < $n) {
  $res = _append($res, mochi_round($a[$k][$m - 1], 5));
  $k = $k + 1;
};
  return $res;
};
  function test_solver() {
  $a = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]];
  $r1 = solve_simultaneous($a);
  if (!(count($r1) == 2 && $r1[0] == (0.0 - 1.0) && $r1[1] == 2.0)) {
  $panic('test1 failed');
}
  $b = [[0.0, (0.0 - 3.0), 1.0, 7.0], [3.0, 2.0, (0.0 - 1.0), 11.0], [5.0, 1.0, (0.0 - 2.0), 12.0]];
  $r2 = solve_simultaneous($b);
  if (!(count($r2) == 3 && $r2[0] == 6.4 && $r2[1] == 1.2 && $r2[2] == 10.6)) {
  $panic('test2 failed');
}
};
  function main() {
  test_solver();
  $eq = [[2.0, 1.0, 1.0, 1.0, 1.0, 4.0], [1.0, 2.0, 1.0, 1.0, 1.0, 5.0], [1.0, 1.0, 2.0, 1.0, 1.0, 6.0], [1.0, 1.0, 1.0, 2.0, 1.0, 7.0], [1.0, 1.0, 1.0, 1.0, 2.0, 8.0]];
  echo rtrim(_str(solve_simultaneous($eq))), PHP_EOL;
  echo rtrim(_str(solve_simultaneous([[4.0, 2.0]]))), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
