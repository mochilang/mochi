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
  function push_relabel_max_flow($graph, $sources, $sinks) {
  global $result;
  if (count($sources) == 0 || count($sinks) == 0) {
  return 0;
}
  $g = $graph;
  $source_index = $sources[0];
  $sink_index = $sinks[0];
  if (count($sources) > 1 || count($sinks) > 1) {
  $max_input_flow = 0;
  $i = 0;
  while ($i < count($sources)) {
  $j = 0;
  while ($j < count($g[$sources[$i]])) {
  $max_input_flow = $max_input_flow + $g[$sources[$i]][$j];
  $j = $j + 1;
};
  $i = $i + 1;
};
  $size = count($g) + 1;
  $new_graph = [];
  $zero_row = [];
  $j = 0;
  while ($j < $size) {
  $zero_row = _append($zero_row, 0);
  $j = $j + 1;
};
  $new_graph = _append($new_graph, $zero_row);
  $r = 0;
  while ($r < count($g)) {
  $row = [0];
  $c = 0;
  while ($c < count($g[$r])) {
  $row = _append($row, $g[$r][$c]);
  $c = $c + 1;
};
  $new_graph = _append($new_graph, $row);
  $r = $r + 1;
};
  $g = $new_graph;
  $i = 0;
  while ($i < count($sources)) {
  $g[0][$sources[$i] + 1] = $max_input_flow;
  $i = $i + 1;
};
  $source_index = 0;
  $size = count($g) + 1;
  $new_graph = [];
  $r = 0;
  while ($r < count($g)) {
  $row2 = $g[$r];
  $row2 = _append($row2, 0);
  $new_graph = _append($new_graph, $row2);
  $r = $r + 1;
};
  $last_row = [];
  $j = 0;
  while ($j < $size) {
  $last_row = _append($last_row, 0);
  $j = $j + 1;
};
  $new_graph = _append($new_graph, $last_row);
  $g = $new_graph;
  $i = 0;
  while ($i < count($sinks)) {
  $g[$sinks[$i] + 1][$size - 1] = $max_input_flow;
  $i = $i + 1;
};
  $sink_index = $size - 1;
}
  $n = count($g);
  $preflow = [];
  $i = 0;
  while ($i < $n) {
  $row = [];
  $j = 0;
  while ($j < $n) {
  $row = _append($row, 0);
  $j = $j + 1;
};
  $preflow = _append($preflow, $row);
  $i = $i + 1;
};
  $heights = [];
  $i = 0;
  while ($i < $n) {
  $heights = _append($heights, 0);
  $i = $i + 1;
};
  $excesses = [];
  $i = 0;
  while ($i < $n) {
  $excesses = _append($excesses, 0);
  $i = $i + 1;
};
  $heights[$source_index] = $n;
  $i = 0;
  while ($i < $n) {
  $bandwidth = $g[$source_index][$i];
  $preflow[$source_index][$i] = $preflow[$source_index][$i] + $bandwidth;
  $preflow[$i][$source_index] = $preflow[$i][$source_index] - $bandwidth;
  $excesses[$i] = $excesses[$i] + $bandwidth;
  $i = $i + 1;
};
  $vertices_list = [];
  $i = 0;
  while ($i < $n) {
  if ($i != $source_index && $i != $sink_index) {
  $vertices_list = _append($vertices_list, $i);
}
  $i = $i + 1;
};
  $idx = 0;
  while ($idx < count($vertices_list)) {
  $v = $vertices_list[$idx];
  $prev_height = $heights[$v];
  while ($excesses[$v] > 0) {
  $nb = 0;
  while ($nb < $n) {
  if ($g[$v][$nb] - $preflow[$v][$nb] > 0 && $heights[$v] > $heights[$nb]) {
  $delta = $excesses[$v];
  $capacity = $g[$v][$nb] - $preflow[$v][$nb];
  if ($delta > $capacity) {
  $delta = $capacity;
};
  $preflow[$v][$nb] = $preflow[$v][$nb] + $delta;
  $preflow[$nb][$v] = $preflow[$nb][$v] - $delta;
  $excesses[$v] = $excesses[$v] - $delta;
  $excesses[$nb] = $excesses[$nb] + $delta;
}
  $nb = $nb + 1;
};
  $min_height = -1;
  $nb = 0;
  while ($nb < $n) {
  if ($g[$v][$nb] - $preflow[$v][$nb] > 0) {
  if ($min_height == (-1) || $heights[$nb] < $min_height) {
  $min_height = $heights[$nb];
};
}
  $nb = $nb + 1;
};
  if ($min_height != (-1)) {
  $heights[$v] = $min_height + 1;
} else {
  break;
}
};
  if ($heights[$v] > $prev_height) {
  $vertex = $vertices_list[$idx];
  $j = $idx;
  while ($j > 0) {
  $vertices_list[$j] = $vertices_list[$j - 1];
  $j = $j - 1;
};
  $vertices_list[0] = $vertex;
  $idx = 0;
} else {
  $idx = $idx + 1;
}
};
  $flow = 0;
  $i = 0;
  while ($i < $n) {
  $flow = $flow + $preflow[$source_index][$i];
  $i = $i + 1;
};
  if ($flow < 0) {
  $flow = -$flow;
}
  return $flow;
};
  $graph = [[0, 7, 0, 0], [0, 0, 6, 0], [0, 0, 0, 8], [9, 0, 0, 0]];
  $sources = [0];
  $sinks = [3];
  $result = push_relabel_max_flow($graph, $sources, $sinks);
  echo rtrim('maximum flow is ' . _str($result)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
