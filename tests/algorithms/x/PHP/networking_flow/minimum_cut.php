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
  function bfs($graph, $s, $t, &$parent) {
  global $test_graph, $result;
  $visited = [];
  $i = 0;
  while ($i < count($graph)) {
  $visited = _append($visited, false);
  $i = $i + 1;
};
  $queue = [$s];
  $head = 0;
  $visited[$s] = true;
  while ($head < count($queue)) {
  $u = $queue[$head];
  $head = $head + 1;
  $ind = 0;
  while ($ind < count($graph[$u])) {
  if ($visited[$ind] == false && $graph[$u][$ind] > 0) {
  $queue = _append($queue, $ind);
  $visited[$ind] = true;
  $parent[$ind] = $u;
}
  $ind = $ind + 1;
};
};
  return $visited[$t];
};
  function mincut($graph, $source, $sink) {
  global $test_graph, $result;
  $g = $graph;
  $parent = [];
  $i = 0;
  while ($i < count($g)) {
  $parent = _append($parent, -1);
  $i = $i + 1;
};
  $temp = [];
  $i = 0;
  while ($i < count($g)) {
  $row = [];
  $j = 0;
  while ($j < count($g[$i])) {
  $row = _append($row, $g[$i][$j]);
  $j = $j + 1;
};
  $temp = _append($temp, $row);
  $i = $i + 1;
};
  while (bfs($g, $source, $sink, $parent)) {
  $path_flow = 1000000000;
  $s = $sink;
  while ($s != $source) {
  $p = $parent[$s];
  $cap = $g[$p][$s];
  if ($cap < $path_flow) {
  $path_flow = $cap;
}
  $s = $p;
};
  $v = $sink;
  while ($v != $source) {
  $u = $parent[$v];
  $g[$u][$v] = $g[$u][$v] - $path_flow;
  $g[$v][$u] = $g[$v][$u] + $path_flow;
  $v = $u;
};
};
  $res = [];
  $i = 0;
  while ($i < count($g)) {
  $j = 0;
  while ($j < count($g[0])) {
  if ($g[$i][$j] == 0 && $temp[$i][$j] > 0) {
  $res = _append($res, [$i, $j]);
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return $res;
};
  $test_graph = [[0, 16, 13, 0, 0, 0], [0, 0, 10, 12, 0, 0], [0, 4, 0, 0, 14, 0], [0, 0, 9, 0, 0, 20], [0, 0, 0, 7, 0, 4], [0, 0, 0, 0, 0, 0]];
  $result = mincut($test_graph, 0, 5);
  echo rtrim(_str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
