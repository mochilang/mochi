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
$__start_mem = memory_get_usage();
$__start = _now();
  function rank_of_matrix(&$matrix) {
  $rows = count($matrix);
  if ($rows == 0) {
  return 0;
}
  $columns = (count($matrix[0]) > 0 ? count($matrix[0]) : 0);
  $rank = ($rows < $columns ? $rows : $columns);
  $row = 0;
  while ($row < $rank) {
  if ($matrix[$row][$row] != 0.0) {
  $col = $row + 1;
  while ($col < $rows) {
  $mult = $matrix[$col][$row] / $matrix[$row][$row];
  $i = $row;
  while ($i < $columns) {
  $matrix[$col][$i] = $matrix[$col][$i] - $mult * $matrix[$row][$i];
  $i = $i + 1;
};
  $col = $col + 1;
};
} else {
  $reduce = true;
  $i = $row + 1;
  while ($i < $rows) {
  if ($matrix[$i][$row] != 0.0) {
  $temp = $matrix[$row];
  $matrix[$row] = $matrix[$i];
  $matrix[$i] = $temp;
  $reduce = false;
  break;
}
  $i = $i + 1;
};
  if ($reduce) {
  $rank = $rank - 1;
  $j = 0;
  while ($j < $rows) {
  $matrix[$j][$row] = $matrix[$j][$rank];
  $j = $j + 1;
};
};
  $row = $row - 1;
}
  $row = $row + 1;
};
  return $rank;
};
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
