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
  function sqrt($x) {
  global $dataset, $value_array, $neighbors, $k, $n;
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
  function euclidean($a, $b) {
  global $dataset, $value_array, $neighbors, $k, $n;
  $sum = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $diff = $a[$i] - $b[$i];
  $sum = $sum + $diff * $diff;
  $i = $i + 1;
};
  $res = sqrt($sum);
  return $res;
};
  function similarity_search($dataset, $value_array) {
  global $neighbors, $k, $n;
  $dim = count($dataset[0]);
  if ($dim != count($value_array[0])) {
  return [];
}
  $result = [];
  $i = 0;
  while ($i < count($value_array)) {
  $value = $value_array[$i];
  $dist = euclidean($value, $dataset[0]);
  $vec = $dataset[0];
  $j = 1;
  while ($j < count($dataset)) {
  $d = euclidean($value, $dataset[$j]);
  if ($d < $dist) {
  $dist = $d;
  $vec = $dataset[$j];
}
  $j = $j + 1;
};
  $nb = ['vector' => $vec, 'distance' => $dist];
  $result = _append($result, $nb);
  $i = $i + 1;
};
  return $result;
};
  function cosine_similarity($a, $b) {
  global $dataset, $value_array, $neighbors, $k, $n;
  $dot = 0.0;
  $norm_a = 0.0;
  $norm_b = 0.0;
  $i = 0;
  while ($i < count($a)) {
  $dot = $dot + $a[$i] * $b[$i];
  $norm_a = $norm_a + $a[$i] * $a[$i];
  $norm_b = $norm_b + $b[$i] * $b[$i];
  $i = $i + 1;
};
  if ($norm_a == 0.0 || $norm_b == 0.0) {
  return 0.0;
}
  return $dot / (sqrt($norm_a) * sqrt($norm_b));
};
  $dataset = [[0.0, 0.0, 0.0], [1.0, 1.0, 1.0], [2.0, 2.0, 2.0]];
  $value_array = [[0.0, 0.0, 0.0], [0.0, 0.0, 1.0]];
  $neighbors = similarity_search($dataset, $value_array);
  $k = 0;
  while ($k < count($neighbors)) {
  $n = $neighbors[$k];
  echo rtrim('[' . _str($n['vector']) . ', ' . _str($n['distance']) . ']'), PHP_EOL;
  $k = $k + 1;
}
  echo rtrim(_str(cosine_similarity([1.0, 2.0], [6.0, 32.0]))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
