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
  function make_graph($directed) {
  global $d_graph;
  $m = [];
  return ['adj_list' => $m, 'directed' => $directed];
};
  function contains_vertex($m, $v) {
  global $d_graph;
  return array_key_exists($v, $m);
};
  function add_edge($g, $s, $d) {
  global $d_graph;
  $adj = $g['adj_list'];
  if (!$g['directed']) {
  if (contains_vertex($adj, $s) && contains_vertex($adj, $d)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = _append($adj[$d], $s);
} else {
  if (contains_vertex($adj, $s)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = [$s];
} else {
  if (contains_vertex($adj, $d)) {
  $adj[$d] = _append($adj[$d], $s);
  $adj[$s] = [$d];
} else {
  $adj[$s] = [$d];
  $adj[$d] = [$s];
};
};
};
} else {
  if (contains_vertex($adj, $s) && contains_vertex($adj, $d)) {
  $adj[$s] = _append($adj[$s], $d);
} else {
  if (contains_vertex($adj, $s)) {
  $adj[$s] = _append($adj[$s], $d);
  $adj[$d] = [];
} else {
  if (contains_vertex($adj, $d)) {
  $adj[$s] = [$d];
} else {
  $adj[$s] = [$d];
  $adj[$d] = [];
};
};
};
}
  $g['adj_list'] = $adj;
  return $g;
};
  function graph_to_string($g) {
  global $d_graph;
  return _str($g['adj_list']);
};
  $d_graph = make_graph(true);
  $d_graph = add_edge($d_graph, _str(0), _str(1));
  echo rtrim(graph_to_string($d_graph)), PHP_EOL;
  $d_graph = add_edge($d_graph, _str(1), _str(2));
  $d_graph = add_edge($d_graph, _str(1), _str(4));
  $d_graph = add_edge($d_graph, _str(1), _str(5));
  echo rtrim(graph_to_string($d_graph)), PHP_EOL;
  $d_graph = add_edge($d_graph, _str(2), _str(0));
  $d_graph = add_edge($d_graph, _str(2), _str(6));
  $d_graph = add_edge($d_graph, _str(2), _str(7));
  echo rtrim(graph_to_string($d_graph)), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
