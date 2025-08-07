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
  function inverse_of_matrix($matrix) {
  global $m2, $m3;
  if (count($matrix) == 2 && count($matrix[0]) == 2 && count($matrix[1]) == 2) {
  $det = $matrix[0][0] * $matrix[1][1] - $matrix[1][0] * $matrix[0][1];
  if ($det == 0.0) {
  echo rtrim('This matrix has no inverse.'), PHP_EOL;
  return [];
};
  return [[$matrix[1][1] / $det, -$matrix[0][1] / $det], [-$matrix[1][0] / $det, $matrix[0][0] / $det]];
} else {
  if (count($matrix) == 3 && count($matrix[0]) == 3 && count($matrix[1]) == 3 && count($matrix[2]) == 3) {
  $det = $matrix[0][0] * $matrix[1][1] * $matrix[2][2] + $matrix[0][1] * $matrix[1][2] * $matrix[2][0] + $matrix[0][2] * $matrix[1][0] * $matrix[2][1] - ($matrix[0][2] * $matrix[1][1] * $matrix[2][0] + $matrix[0][1] * $matrix[1][0] * $matrix[2][2] + $matrix[0][0] * $matrix[1][2] * $matrix[2][1]);
  if ($det == 0.0) {
  echo rtrim('This matrix has no inverse.'), PHP_EOL;
  return [];
};
  $cof = [[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]];
  $cof[0][0] = $matrix[1][1] * $matrix[2][2] - $matrix[1][2] * $matrix[2][1];
  $cof[0][1] = -($matrix[1][0] * $matrix[2][2] - $matrix[1][2] * $matrix[2][0]);
  $cof[0][2] = $matrix[1][0] * $matrix[2][1] - $matrix[1][1] * $matrix[2][0];
  $cof[1][0] = -($matrix[0][1] * $matrix[2][2] - $matrix[0][2] * $matrix[2][1]);
  $cof[1][1] = $matrix[0][0] * $matrix[2][2] - $matrix[0][2] * $matrix[2][0];
  $cof[1][2] = -($matrix[0][0] * $matrix[2][1] - $matrix[0][1] * $matrix[2][0]);
  $cof[2][0] = $matrix[0][1] * $matrix[1][2] - $matrix[0][2] * $matrix[1][1];
  $cof[2][1] = -($matrix[0][0] * $matrix[1][2] - $matrix[0][2] * $matrix[1][0]);
  $cof[2][2] = $matrix[0][0] * $matrix[1][1] - $matrix[0][1] * $matrix[1][0];
  $inv = [[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]];
  $i = 0;
  while ($i < 3) {
  $j = 0;
  while ($j < 3) {
  $inv[$i][$j] = $cof[$j][$i] / $det;
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $inv;
};
}
  echo rtrim('Please provide a matrix of size 2x2 or 3x3.'), PHP_EOL;
  return [];
};
  $m2 = [[2.0, 5.0], [2.0, 0.0]];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inverse_of_matrix($m2), 1344)))))), PHP_EOL;
  $m3 = [[2.0, 5.0, 7.0], [2.0, 0.0, 1.0], [1.0, 2.0, 3.0]];
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode(inverse_of_matrix($m3), 1344)))))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
