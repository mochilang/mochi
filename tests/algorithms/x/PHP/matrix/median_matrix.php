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
  function bubble_sort($a) {
  global $matrix1, $matrix2;
  $arr = $a;
  $n = count($arr);
  $i = 0;
  while ($i < $n) {
  $j = 0;
  while ($j + 1 < $n - $i) {
  if ($arr[$j] > $arr[$j + 1]) {
  $temp = $arr[$j];
  $arr[$j] = $arr[$j + 1];
  $arr[$j + 1] = $temp;
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $arr;
};
  function median($matrix) {
  global $matrix1, $matrix2;
  $linear = [];
  $i = 0;
  while ($i < count($matrix)) {
  $row = $matrix[$i];
  $j = 0;
  while ($j < count($row)) {
  $linear = _append($linear, $row[$j]);
  $j = $j + 1;
};
  $i = $i + 1;
};
  $sorted = bubble_sort($linear);
  $mid = (count($sorted) - 1) / 2;
  return $sorted[$mid];
};
  $matrix1 = [[1, 3, 5], [2, 6, 9], [3, 6, 9]];
  echo rtrim(_str(median($matrix1))), PHP_EOL;
  $matrix2 = [[1, 2, 3], [4, 5, 6]];
  echo rtrim(_str(median($matrix2))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
