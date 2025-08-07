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
$__start_mem = memory_get_usage();
$__start = _now();
  function get_winner($weights, $sample) {
  $d0 = 0.0;
  $d1 = 0.0;
  for ($i = 0; $i < count($sample); $i++) {
  $diff0 = $sample[$i] - $weights[0][$i];
  $diff1 = $sample[$i] - $weights[1][$i];
  $d0 = $d0 + $diff0 * $diff0;
  $d1 = $d1 + $diff1 * $diff1;
  return ($d0 > $d1 ? 0 : 1);
};
  return 0;
};
  function update(&$weights, $sample, $j, $alpha) {
  for ($i = 0; $i < count($weights); $i++) {
  $weights[$j][$i] = $weights[$j][$i] + $alpha * ($sample[$i] - $weights[$j][$i]);
};
  return $weights;
};
  function list_to_string($xs) {
  $s = '[';
  $i = 0;
  while ($i < count($xs)) {
  $s = $s . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function matrix_to_string($m) {
  $s = '[';
  $i = 0;
  while ($i < count($m)) {
  $s = $s . list_to_string($m[$i]);
  if ($i < count($m) - 1) {
  $s = $s . ', ';
}
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
};
  function main() {
  $training_samples = [[1, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 1]];
  $weights = [[0.2, 0.6, 0.5, 0.9], [0.8, 0.4, 0.7, 0.3]];
  $epochs = 3;
  $alpha = 0.5;
  for ($_ = 0; $_ < $epochs; $_++) {
  for ($j = 0; $j < count($training_samples); $j++) {
  $sample = $training_samples[$j];
  $winner = get_winner($weights, $sample);
  $weights = update($weights, $sample, $winner, $alpha);
};
};
  $sample = [0, 0, 0, 1];
  $winner = get_winner($weights, $sample);
  echo rtrim('Clusters that the test sample belongs to : ' . _str($winner)), PHP_EOL;
  echo rtrim('Weights that have been trained : ' . matrix_to_string($weights)), PHP_EOL;
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
