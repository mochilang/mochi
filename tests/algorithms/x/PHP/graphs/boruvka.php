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
  function new_graph($num_nodes) {
  return ['num_nodes' => $num_nodes, 'edges' => [], 'component' => []];
};
  function add_edge($g, $u, $v, $w) {
  $es = $g['edges'];
  $es = _append($es, ['u' => $u, 'v' => $v, 'w' => $w]);
  return ['num_nodes' => $g['num_nodes'], 'edges' => $es, 'component' => $g['component']];
};
  function find_component($g, $node) {
  if ($g['component'][$node] == $node) {
  return $node;
}
  return find_component($g, $g['component'][$node]);
};
  function set_component($g, $node) {
  if ($g['component'][$node] != $node) {
  $comp = $g['component'];
  $k = 0;
  while ($k < $g['num_nodes']) {
  $comp[$k] = find_component($g, $k);
  $k = $k + 1;
};
  $g = ['num_nodes' => $g['num_nodes'], 'edges' => $g['edges'], 'component' => $comp];
}
  return $g;
};
  function union($g, $component_size, $u, $v) {
  $comp_size = $component_size;
  $comp = $g['component'];
  if ($comp_size[$u] <= $comp_size[$v]) {
  $comp[$u] = $v;
  $comp_size[$v] = $comp_size[$v] + $comp_size[$u];
  $g = ['num_nodes' => $g['num_nodes'], 'edges' => $g['edges'], 'component' => $comp];
  $g = set_component($g, $u);
} else {
  $comp[$v] = $u;
  $comp_size[$u] = $comp_size[$u] + $comp_size[$v];
  $g = ['num_nodes' => $g['num_nodes'], 'edges' => $g['edges'], 'component' => $comp];
  $g = set_component($g, $v);
}
  return ['graph' => $g, 'component_size' => $comp_size];
};
  function create_empty_edges($n) {
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = _append($res, ['u' => 0 - 1, 'v' => 0 - 1, 'w' => 0 - 1]);
  $i = $i + 1;
};
  return $res;
};
  function boruvka($g) {
  $component_size = [];
  $i = 0;
  while ($i < $g['num_nodes']) {
  $component_size = _append($component_size, 1);
  $comp = $g['component'];
  $comp[$i] = $i;
  $g = ['num_nodes' => $g['num_nodes'], 'edges' => $g['edges'], 'component' => $comp];
  $i = $i + 1;
};
  $mst_weight = 0;
  $num_components = $g['num_nodes'];
  $minimum_weight_edge = create_empty_edges($g['num_nodes']);
  while ($num_components > 1) {
  foreach ($g['edges'] as $e) {
  $u = $e['u'];
  $v = $e['v'];
  $w = $e['w'];
  $u_comp = $g['component'][$u];
  $v_comp = $g['component'][$v];
  if ($u_comp != $v_comp) {
  $current_u = $minimum_weight_edge[$u_comp];
  if ($current_u['u'] == 0 - 1 || $current_u['w'] > $w) {
  $minimum_weight_edge[$u_comp] = ['u' => $u, 'v' => $v, 'w' => $w];
};
  $current_v = $minimum_weight_edge[$v_comp];
  if ($current_v['u'] == 0 - 1 || $current_v['w'] > $w) {
  $minimum_weight_edge[$v_comp] = ['u' => $u, 'v' => $v, 'w' => $w];
};
}
};
  foreach ($minimum_weight_edge as $e) {
  if ($e['u'] != 0 - 1) {
  $u = $e['u'];
  $v = $e['v'];
  $w = $e['w'];
  $u_comp = $g['component'][$u];
  $v_comp = $g['component'][$v];
  if ($u_comp != $v_comp) {
  $mst_weight = $mst_weight + $w;
  $res = union($g, $component_size, $u_comp, $v_comp);
  $g = $res['graph'];
  $component_size = $res['component_size'];
  echo rtrim('Added edge [' . _str($u) . ' - ' . _str($v) . ']'), PHP_EOL;
  echo rtrim('Added weight: ' . _str($w)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $num_components = $num_components - 1;
};
}
};
  $minimum_weight_edge = create_empty_edges($g['num_nodes']);
};
  echo rtrim('The total weight of the minimal spanning tree is: ' . _str($mst_weight)), PHP_EOL;
  return $mst_weight;
};
  function main() {
  $g = new_graph(8);
  $edges = [[0, 1, 10], [0, 2, 6], [0, 3, 5], [1, 3, 15], [2, 3, 4], [3, 4, 8], [4, 5, 10], [4, 6, 6], [4, 7, 5], [5, 7, 15], [6, 7, 4]];
  foreach ($edges as $e) {
  $g = add_edge($g, $e[0], $e[1], $e[2]);
};
  boruvka($g);
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
