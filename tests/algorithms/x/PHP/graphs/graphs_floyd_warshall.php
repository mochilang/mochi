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
  $INF = 1000000000.0;
  function floyd_warshall($graph) {
  global $INF, $result;
  $v = count($graph);
  $dist = [];
  $i = 0;
  while ($i < $v) {
  $row = [];
  $j = 0;
  while ($j < $v) {
  $row = _append($row, $graph[$i][$j]);
  $j = $j + 1;
};
  $dist = _append($dist, $row);
  $i = $i + 1;
};
  $k = 0;
  while ($k < $v) {
  $i = 0;
  while ($i < $v) {
  $j = 0;
  while ($j < $v) {
  if ($dist[$i][$k] < $INF && $dist[$k][$j] < $INF && $dist[$i][$k] + $dist[$k][$j] < $dist[$i][$j]) {
  $dist[$i][$j] = $dist[$i][$k] + $dist[$k][$j];
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  $k = $k + 1;
};
  return $dist;
};
  function print_dist($dist) {
  global $INF, $graph, $result;
  echo rtrim('
The shortest path matrix using Floyd Warshall algorithm
'), PHP_EOL;
  $i = 0;
  while ($i < count($dist)) {
  $j = 0;
  $line = '';
  while ($j < count($dist[$i])) {
  if ($dist[$i][$j] >= $INF / 2.0) {
  $line = $line . 'INF	';
} else {
  $line = $line . _str(intval($dist[$i][$j])) . '	';
}
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
};
  $graph = [[0.0, 5.0, $INF, 10.0], [$INF, 0.0, 3.0, $INF], [$INF, $INF, 0.0, 1.0], [$INF, $INF, $INF, 0.0]];
  $result = floyd_warshall($graph);
  print_dist($result);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
