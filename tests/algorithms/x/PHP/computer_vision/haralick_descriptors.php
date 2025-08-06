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
  function abs_int($n) {
  global $image, $glcm, $descriptors, $idx;
  if ($n < 0) {
  return -$n;
}
  return $n;
};
  function sqrt($x) {
  global $image, $glcm, $descriptors, $idx;
  if ($x <= 0.0) {
  return 0.0;
}
  $guess = $x;
  $i = 0;
  while ($i < 10) {
  $guess = ($guess + $x / $guess) / 2.0;
  $i = $i + 1;
};
  return $guess;
};
  function ln($x) {
  global $image, $glcm, $descriptors, $idx;
  if ($x <= 0.0) {
  return 0.0;
}
  $e = 2.718281828;
  $n = 0;
  $y = $x;
  while ($y >= $e) {
  $y = $y / $e;
  $n = $n + 1;
};
  while ($y <= 1.0 / $e) {
  $y = $y * $e;
  $n = $n - 1;
};
  $y = $y - 1.0;
  $term = $y;
  $result = 0.0;
  $k = 1;
  while ($k <= 20) {
  if ($k % 2 == 1) {
  $result = $result + $term / (1.0 * $k);
} else {
  $result = $result - $term / (1.0 * $k);
}
  $term = $term * $y;
  $k = $k + 1;
};
  return $result + (1.0 * $n);
};
  function matrix_concurrency($image, $coord) {
  global $glcm, $descriptors, $idx;
  $offset_x = $coord[0];
  $offset_y = $coord[1];
  $max_val = 0;
  for ($r = 0; $r < count($image); $r++) {
  for ($c = 0; $c < count($image[$r]); $c++) {
  if ($image[$r][$c] > $max_val) {
  $max_val = $image[$r][$c];
}
};
};
  $size = $max_val + 1;
  $matrix = [];
  for ($i = 0; $i < $size; $i++) {
  $row = [];
  for ($j = 0; $j < $size; $j++) {
  $row = _append($row, 0.0);
};
  $matrix = _append($matrix, $row);
};
  for ($x = 1; $x < count($image) - 1; $x++) {
  for ($y = 1; $y < count($image[$x]) - 1; $y++) {
  $base = $image[$x][$y];
  $offset = $image[$x + $offset_x][$y + $offset_y];
  $matrix[$base][$offset] = $matrix[$base][$offset] + 1.0;
};
};
  $total = 0.0;
  for ($i = 0; $i < $size; $i++) {
  for ($j = 0; $j < $size; $j++) {
  $total = $total + $matrix[$i][$j];
};
};
  if ($total == 0.0) {
  return $matrix;
}
  for ($i = 0; $i < $size; $i++) {
  for ($j = 0; $j < $size; $j++) {
  $matrix[$i][$j] = $matrix[$i][$j] / $total;
};
};
  return $matrix;
};
  function haralick_descriptors($matrix) {
  global $image, $glcm, $descriptors, $idx;
  $rows = count($matrix);
  $cols = count($matrix[0]);
  $maximum_prob = 0.0;
  $correlation = 0.0;
  $energy = 0.0;
  $contrast = 0.0;
  $dissimilarity = 0.0;
  $inverse_difference = 0.0;
  $homogeneity = 0.0;
  $entropy = 0.0;
  $i = 0;
  while ($i < $rows) {
  $j = 0;
  while ($j < $cols) {
  $val = $matrix[$i][$j];
  if ($val > $maximum_prob) {
  $maximum_prob = $val;
}
  $correlation = $correlation + (1.0 * $i * $j) * $val;
  $energy = $energy + $val * $val;
  $diff = $i - $j;
  $adiff = abs_int($diff);
  $contrast = $contrast + $val * (1.0 * $diff * $diff);
  $dissimilarity = $dissimilarity + $val * (1.0 * $adiff);
  $inverse_difference = $inverse_difference + $val / (1.0 + (1.0 * $adiff));
  $homogeneity = $homogeneity + $val / (1.0 + (1.0 * $diff * $diff));
  if ($val > 0.0) {
  $entropy = $entropy - ($val * ln($val));
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return [$maximum_prob, $correlation, $energy, $contrast, $dissimilarity, $inverse_difference, $homogeneity, $entropy];
};
  $image = [[0, 1, 0], [1, 0, 1], [0, 1, 0]];
  $glcm = matrix_concurrency($image, [0, 1]);
  $descriptors = haralick_descriptors($glcm);
  $idx = 0;
  while ($idx < count($descriptors)) {
  echo rtrim(_str($descriptors[$idx])), PHP_EOL;
  $idx = $idx + 1;
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
