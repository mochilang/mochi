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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function invert_matrix($matrix) {
  global $mat;
  $n = count($matrix);
  $aug = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, $matrix[$i][$j]);
  $j = $j + 1;
};
  $k = 0;
  while ($k < $n) {
  if ($i == $k) {
  $row = _append($row, 1.0);
} else {
  $row = _append($row, 0.0);
}
  $k = $k + 1;
};
  $aug = _append($aug, $row);
  $i = $i + 1;
};
  $col = 0;
  while ($col < $n) {
  $pivot_row = $col;
  $r = $col;
  while ($r < $n) {
  if ($aug[$r][$col] != 0.0) {
  $pivot_row = $r;
  break;
}
  $r = $r + 1;
};
  if ($aug[$pivot_row][$col] == 0.0) {
  $panic('Matrix is not invertible');
}
  if ($pivot_row != $col) {
  $temp = $aug[$col];
  $aug[$col] = $aug[$pivot_row];
  $aug[$pivot_row] = $temp;
}
  $pivot = $aug[$col][$col];
  $c = 0;
  while ($c < 2 * $n) {
  $aug[$col][$c] = $aug[$col][$c] / $pivot;
  $c = $c + 1;
};
  $r2 = 0;
  while ($r2 < $n) {
  if ($r2 != $col) {
  $factor = $aug[$r2][$col];
  $c2 = 0;
  while ($c2 < 2 * $n) {
  $aug[$r2][$c2] = $aug[$r2][$c2] - $factor * $aug[$col][$c2];
  $c2 = $c2 + 1;
};
}
  $r2 = $r2 + 1;
};
  $col = $col + 1;
};
  $inv = [];
  $r3 = 0;
  while ($r3 < $n) {
  $row = [];
  $c3 = 0;
  while ($c3 < $n) {
  $row = _append($row, $aug[$r3][$c3 + $n]);
  $c3 = $c3 + 1;
};
  $inv = _append($inv, $row);
  $r3 = $r3 + 1;
};
  return $inv;
};
  $mat = [[4.0, 7.0], [2.0, 6.0]];
  echo rtrim('Original Matrix:'), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($mat, 1344)))))), PHP_EOL;
  echo rtrim('Inverted Matrix:'), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(invert_matrix($mat), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
