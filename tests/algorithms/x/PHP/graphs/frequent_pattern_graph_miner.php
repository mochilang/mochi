<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
$EDGE_ARRAY = [[['ab', 'e1'], ['ac', 'e3'], ['ad', 'e5'], ['bc', 'e4'], ['bd', 'e2'], ['be', 'e6'], ['bh', 'e12'], ['cd', 'e2'], ['ce', 'e4'], ['de', 'e1'], ['df', 'e8'], ['dg', 'e5'], ['dh', 'e10'], ['ef', 'e3'], ['eg', 'e2'], ['fg', 'e6'], ['gh', 'e6'], ['hi', 'e3']], [['ab', 'e1'], ['ac', 'e3'], ['ad', 'e5'], ['bc', 'e4'], ['bd', 'e2'], ['be', 'e6'], ['cd', 'e2'], ['de', 'e1'], ['df', 'e8'], ['ef', 'e3'], ['eg', 'e2'], ['fg', 'e6']], [['ab', 'e1'], ['ac', 'e3'], ['bc', 'e4'], ['bd', 'e2'], ['de', 'e1'], ['df', 'e8'], ['dg', 'e5'], ['ef', 'e3'], ['eg', 'e2'], ['eh', 'e12'], ['fg', 'e6'], ['fh', 'e10'], ['gh', 'e6']], [['ab', 'e1'], ['ac', 'e3'], ['bc', 'e4'], ['bd', 'e2'], ['bh', 'e12'], ['cd', 'e2'], ['df', 'e8'], ['dh', 'e10']], [['ab', 'e1'], ['ac', 'e3'], ['ad', 'e5'], ['bc', 'e4'], ['bd', 'e2'], ['cd', 'e2'], ['ce', 'e4'], ['de', 'e1'], ['df', 'e8'], ['dg', 'e5'], ['ef', 'e3'], ['eg', 'e2'], ['fg', 'e6']]];
function contains($lst, $item) {
  global $EDGE_ARRAY, $paths;
  foreach ($lst as $v) {
  if ($v == $item) {
  return true;
}
};
  return false;
}
function get_distinct_edge($edge_array) {
  global $EDGE_ARRAY, $paths;
  $distinct = [];
  foreach ($edge_array as $row) {
  foreach ($row as $item) {
  $e = $item[0];
  if (!contains($distinct, $e)) {
  $distinct = _append($distinct, $e);
}
};
};
  return $distinct;
}
function get_bitcode($edge_array, $de) {
  global $EDGE_ARRAY, $paths;
  $bitcode = '';
  $i = 0;
  while ($i < count($edge_array)) {
  $found = false;
  foreach ($edge_array[$i] as $item) {
  if ($item[0] == $de) {
  $found = true;
  break;
}
};
  if ($found) {
  $bitcode = $bitcode . '1';
} else {
  $bitcode = $bitcode . '0';
}
  $i = $i + 1;
};
  return $bitcode;
}
function count_ones($s) {
  global $EDGE_ARRAY, $paths;
  $c = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == '1') {
  $c = $c + 1;
}
  $i = $i + 1;
};
  return $c;
}
function get_frequency_table($edge_array) {
  global $EDGE_ARRAY, $paths;
  $distinct = get_distinct_edge($edge_array);
  $table = [];
  foreach ($distinct as $e) {
  $bit = get_bitcode($edge_array, $e);
  $cnt = count_ones($bit);
  $entry = ['edge' => $e, 'count' => _str($cnt), 'bit' => $bit];
  $table = _append($table, $entry);
};
  $i = 0;
  while ($i < count($table)) {
  $max_i = $i;
  $j = $i + 1;
  while ($j < count($table)) {
  if ($toi($table[$j]['count']) > $toi($table[$max_i]['count'])) {
  $max_i = $j;
}
  $j = $j + 1;
};
  $tmp = $table[$i];
  $table[$i] = $table[$max_i];
  $table[$max_i] = $tmp;
  $i = $i + 1;
};
  return $table;
}
function get_nodes($freq_table) {
  global $EDGE_ARRAY, $paths;
  $nodes = [];
  $keys = [];
  foreach ($freq_table as $f) {
  $code = $f['bit'];
  $edge = $f['edge'];
  if (array_key_exists($code, $nodes)) {
  $nodes[$code] = _append($nodes[$code], $edge);
} else {
  $nodes[$code] = [$edge];
  $keys = _append($keys, $code);
}
};
  return ['map' => $nodes, 'keys' => $keys];
}
function get_cluster($nodes) {
  global $EDGE_ARRAY, $paths;
  $clusters = [];
  $weights = [];
  $i = 0;
  while ($i < _len($nodes['keys'])) {
  $code = $nodes['keys'][$i];
  $wt = count_ones($code);
  if (array_key_exists($wt, $clusters)) {
  $clusters[$wt] = _append($clusters[$wt], $code);
} else {
  $clusters[$wt] = [$code];
  $weights = _append($weights, $wt);
}
  $i = $i + 1;
};
  return ['clusters' => $clusters, 'weights' => $weights];
}
function get_support($clusters) {
  global $EDGE_ARRAY, $paths;
  $sup = [];
  $i = 0;
  while ($i < _len($clusters['weights'])) {
  $w = $clusters['weights'][$i];
  $sup = _append($sup, $w * 100 / _len($clusters['weights']));
  $i = $i + 1;
};
  return $sup;
}
function contains_bits($a, $b) {
  global $EDGE_ARRAY, $paths;
  $i = 0;
  while ($i < strlen($a)) {
  $c1 = substr($a, $i, $i + 1 - $i);
  $c2 = substr($b, $i, $i + 1 - $i);
  if ($c1 == '1' && $c2 != '1') {
  return false;
}
  $i = $i + 1;
};
  return true;
}
function max_cluster_key($clusters) {
  global $EDGE_ARRAY, $paths;
  $m = 0;
  $i = 0;
  while ($i < _len($clusters['weights'])) {
  $w = $clusters['weights'][$i];
  if ($w > $m) {
  $m = $w;
}
  $i = $i + 1;
};
  return $m;
}
function get_cluster_codes($clusters, $wt) {
  global $EDGE_ARRAY, $paths;
  if (isset($clusters['clusters'][$wt])) {
  return $clusters['clusters'][$wt];
}
  return [];
}
function create_edge($nodes, &$graph, $gkeys, $clusters, $c1, $maxk) {
  global $EDGE_ARRAY, $paths;
  $keys = $gkeys;
  $codes1 = get_cluster_codes($clusters, $c1);
  $idx1 = 0;
  while ($idx1 < count($codes1)) {
  $i_code = $codes1[$idx1];
  $count = 0;
  $c2 = $c1 + 1;
  while ($c2 <= $maxk) {
  $codes2 = get_cluster_codes($clusters, $c2);
  $j = 0;
  while ($j < count($codes2)) {
  $j_code = $codes2[$j];
  if (contains_bits($i_code, $j_code)) {
  if (array_key_exists($i_code, $graph)) {
  $graph[$i_code] = _append($graph[$i_code], $j_code);
} else {
  $graph[$i_code] = [$j_code];
  if (!contains($keys, $i_code)) {
  $keys = _append($keys, $i_code);
};
};
  if (!contains($keys, $j_code)) {
  $keys = _append($keys, $j_code);
};
  $count = $count + 1;
}
  $j = $j + 1;
};
  if ($count == 0) {
  $c2 = $c2 + 1;
} else {
  break;
}
};
  $idx1 = $idx1 + 1;
};
  return $keys;
}
function construct_graph($clusters, $nodes) {
  global $EDGE_ARRAY, $paths;
  $maxk = max_cluster_key($clusters);
  $top_codes = get_cluster_codes($clusters, $maxk);
  $graph = [];
  $keys = ['Header'];
  $graph['Header'] = [];
  $i = 0;
  while ($i < count($top_codes)) {
  $code = $top_codes[$i];
  $graph['Header'] = _append($graph['Header'], $code);
  $graph[$code] = ['Header'];
  $keys = _append($keys, $code);
  $i = $i + 1;
};
  $c = 1;
  while ($c < $maxk) {
  $keys = create_edge($nodes, $graph, $keys, $clusters, $c, $maxk);
  $c = $c + 1;
};
  return ['edges' => $graph, 'keys' => $keys];
}
$paths = [];
function copy_list($lst) {
  global $EDGE_ARRAY, $paths;
  $n = [];
  foreach ($lst as $v) {
  $n = _append($n, $v);
};
  return $n;
}
function my_dfs($graph, $start, $end, $path) {
  global $EDGE_ARRAY, $paths;
  $new_path = copy_list($path);
  $new_path = _append($new_path, $start);
  if ($start == $end) {
  $paths = _append($paths, $new_path);
  return;
}
  foreach ($graph[$start] as $node) {
  $seen = false;
  foreach ($new_path as $p) {
  if ($p == $node) {
  $seen = true;
}
};
  if (!$seen) {
  my_dfs($graph, $node, $end, $new_path);
}
};
}
function find_freq_subgraph_given_support($s, $clusters, $graph) {
  global $EDGE_ARRAY, $paths;
  $k = $s * _len($clusters['weights']) / 100;
  $codes = get_cluster_codes($clusters, $k);
  $i = 0;
  while ($i < count($codes)) {
  my_dfs($graph['edges'], $codes[$i], 'Header', []);
  $i = $i + 1;
};
}
function node_edges($nodes, $code) {
  global $EDGE_ARRAY, $paths;
  return $nodes['map'][$code];
}
function freq_subgraphs_edge_list($paths, $nodes) {
  global $EDGE_ARRAY;
  $freq_sub_el = [];
  foreach ($paths as $path) {
  $el = [];
  $j = 0;
  while ($j < count($path) - 1) {
  $code = $path[$j];
  $edge_list = node_edges($nodes, $code);
  $e = 0;
  while ($e < count($edge_list)) {
  $edge = $edge_list[$e];
  $a = substr($edge, 0, 1 - 0);
  $b = substr($edge, 1, 2 - 1);
  $el = _append($el, [$a, $b]);
  $e = $e + 1;
};
  $j = $j + 1;
};
  $freq_sub_el = _append($freq_sub_el, $el);
};
  return $freq_sub_el;
}
function print_all($nodes, $support, $clusters, $graph, $freq_subgraph_edge_list) {
  global $EDGE_ARRAY, $paths;
  echo rtrim('
Nodes
'), PHP_EOL;
  $i = 0;
  while ($i < _len($nodes['keys'])) {
  $code = $nodes['keys'][$i];
  echo rtrim($code), PHP_EOL;
  echo rtrim(json_encode($nodes['map'][$code], 1344)), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim('
Support
'), PHP_EOL;
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($support, 1344)))))), PHP_EOL;
  echo rtrim('
Cluster
'), PHP_EOL;
  $j = 0;
  while ($j < _len($clusters['weights'])) {
  $w = $clusters['weights'][$j];
  echo rtrim(_str($w) . ':' . _str($clusters['clusters'][$w])), PHP_EOL;
  $j = $j + 1;
};
  echo rtrim('
Graph
'), PHP_EOL;
  $k = 0;
  while ($k < _len($graph['keys'])) {
  $key = $graph['keys'][$k];
  echo rtrim($key), PHP_EOL;
  echo rtrim(json_encode($graph['edges'][$key], 1344)), PHP_EOL;
  $k = $k + 1;
};
  echo rtrim('
Edge List of Frequent subgraphs
'), PHP_EOL;
  foreach ($freq_subgraph_edge_list as $el) {
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($el, 1344)))))), PHP_EOL;
};
}
function main() {
  global $EDGE_ARRAY, $paths;
  $frequency_table = get_frequency_table($EDGE_ARRAY);
  $nodes = get_nodes($frequency_table);
  $clusters = get_cluster($nodes);
  $support = get_support($clusters);
  $graph = construct_graph($clusters, $nodes);
  find_freq_subgraph_given_support(60, $clusters, $graph);
  $freq_subgraph_edge_list = freq_subgraphs_edge_list($paths, $nodes);
  print_all($nodes, $support, $clusters, $graph, $freq_subgraph_edge_list);
}
main();
