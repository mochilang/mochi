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
  function assign_ranks($data) {
  $ranks = [];
  $n = count($data);
  $i = 0;
  while ($i < $n) {
  $rank = 1;
  $j = 0;
  while ($j < $n) {
  if ($data[$j] < $data[$i] || ($data[$j] == $data[$i] && $j < $i)) {
  $rank = $rank + 1;
}
  $j = $j + 1;
};
  $ranks = _append($ranks, $rank);
  $i = $i + 1;
};
  return $ranks;
};
  function calculate_spearman_rank_correlation($var1, $var2) {
  if (count($var1) != count($var2)) {
  $panic('Lists must have equal length');
}
  $n = count($var1);
  $rank1 = assign_ranks($var1);
  $rank2 = assign_ranks($var2);
  $i = 0;
  $d_sq = 0.0;
  while ($i < $n) {
  $diff = (floatval(($rank1[$i] - $rank2[$i])));
  $d_sq = $d_sq + $diff * $diff;
  $i = $i + 1;
};
  $n_f = (floatval($n));
  return 1.0 - (6.0 * $d_sq) / ($n_f * ($n_f * $n_f - 1.0));
};
  function test_spearman() {
  $x = [1.0, 2.0, 3.0, 4.0, 5.0];
  $y_inc = [2.0, 4.0, 6.0, 8.0, 10.0];
  if (calculate_spearman_rank_correlation($x, $y_inc) != 1.0) {
  $panic('case1');
}
  $y_dec = [5.0, 4.0, 3.0, 2.0, 1.0];
  if (calculate_spearman_rank_correlation($x, $y_dec) != (-1.0)) {
  $panic('case2');
}
  $y_mix = [5.0, 1.0, 2.0, 9.0, 5.0];
  if (calculate_spearman_rank_correlation($x, $y_mix) != 0.6) {
  $panic('case3');
}
};
  function main() {
  test_spearman();
  echo rtrim(_str(calculate_spearman_rank_correlation([1.0, 2.0, 3.0, 4.0, 5.0], [2.0, 4.0, 6.0, 8.0, 10.0]))), PHP_EOL;
  echo rtrim(_str(calculate_spearman_rank_correlation([1.0, 2.0, 3.0, 4.0, 5.0], [5.0, 4.0, 3.0, 2.0, 1.0]))), PHP_EOL;
  echo rtrim(_str(calculate_spearman_rank_correlation([1.0, 2.0, 3.0, 4.0, 5.0], [5.0, 1.0, 2.0, 9.0, 5.0]))), PHP_EOL;
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
