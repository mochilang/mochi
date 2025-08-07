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
  function identity($n) {
  $mat = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  if ($i == $j) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $j = $j + 1;
};
  $mat = _append($mat, $row);
  $i = $i + 1;
};
  return $mat;
};
  function transpose($mat) {
  $rows = count($mat);
  $cols = count($mat[0]);
  $res = [];
  $j = 0;
  while ($j < $cols) {
  $row = [];
  $i = 0;
  while ($i < $rows) {
  $row = _append($row, $mat[$i][$j]);
  $i = $i + 1;
};
  $res = _append($res, $row);
  $j = $j + 1;
};
  return $res;
};
  function matmul($a, $b) {
  $rows = count($a);
  $cols = count($b[0]);
  $inner = count($a[0]);
  $res = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $sum = 0.0;
  $k = 0;
  while ($k < $inner) {
  $sum = $sum + $a[$i][$k] * $b[$k][$j];
  $k = $k + 1;
};
  $row = _append($row, $sum);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function mat_sub($a, $b) {
  $rows = count($a);
  $cols = count($a[0]);
  $res = [];
  $i = 0;
  while ($i < $rows) {
  $row = [];
  $j = 0;
  while ($j < $cols) {
  $row = _append($row, $a[$i][$j] - $b[$i][$j]);
  $j = $j + 1;
};
  $res = _append($res, $row);
  $i = $i + 1;
};
  return $res;
};
  function inverse($mat) {
  $n = count($mat);
  $id = identity($n);
  $aug = [];
  $i = 0;
  while ($i < $n) {
  $row = array_merge($mat[$i], $id[$i]);
  $aug = _append($aug, $row);
  $i = $i + 1;
};
  $col = 0;
  while ($col < $n) {
  $pivot_row = $aug[$col];
  $pivot = $pivot_row[$col];
  if ($pivot == 0.0) {
  $panic('matrix is singular');
}
  $j = 0;
  while ($j < 2 * $n) {
  $pivot_row[$j] = $pivot_row[$j] / $pivot;
  $j = $j + 1;
};
  $aug[$col] = $pivot_row;
  $r = 0;
  while ($r < $n) {
  if ($r != $col) {
  $row_r = $aug[$r];
  $factor = $row_r[$col];
  $j = 0;
  while ($j < 2 * $n) {
  $row_r[$j] = $row_r[$j] - $factor * $pivot_row[$j];
  $j = $j + 1;
};
  $aug[$r] = $row_r;
}
  $r = $r + 1;
};
  $col = $col + 1;
};
  $inv = [];
  $r = 0;
  while ($r < $n) {
  $row = [];
  $c = $n;
  while ($c < 2 * $n) {
  $row = _append($row, $aug[$r][$c]);
  $c = $c + 1;
};
  $inv = _append($inv, $row);
  $r = $r + 1;
};
  return $inv;
};
  function schur_complement($mat_a, $mat_b, $mat_c, $pseudo_inv) {
  $a_rows = count($mat_a);
  $a_cols = count($mat_a[0]);
  if ($a_rows != $a_cols) {
  $panic('Matrix A must be square');
}
  if ($a_rows != count($mat_b)) {
  $panic('Expected the same number of rows for A and B');
}
  if (count($mat_b[0]) != count($mat_c[0])) {
  $panic('Expected the same number of columns for B and C');
}
  $a_inv = null;
  if ($pseudo_inv['ok']) {
  $a_inv = $pseudo_inv['value'];
} else {
  $a_inv = inverse($mat_a);
}
  $bt = transpose($mat_b);
  $a_inv_b = matmul($a_inv, $mat_b);
  $bt_a_inv_b = matmul($bt, $a_inv_b);
  return mat_sub($mat_c, $bt_a_inv_b);
};
  function print_matrix($mat) {
  $i = 0;
  while ($i < count($mat)) {
  $line = '';
  $j = 0;
  $row = $mat[$i];
  while ($j < count($row)) {
  $line = $line . _str($row[$j]);
  if ($j + 1 < count($row)) {
  $line = $line . ' ';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  function main() {
  $a = [[1.0, 2.0], [2.0, 1.0]];
  $b = [[0.0, 3.0], [3.0, 0.0]];
  $c = [[2.0, 1.0], [6.0, 3.0]];
  $none = ['value' => [], 'ok' => false];
  $s = schur_complement($a, $b, $c, $none);
  print_matrix($s);
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
