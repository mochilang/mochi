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
function node_to_string($n) {
  global $names, $graph, $nodes, $ri, $row, $ci, $n_in, $n_out;
  return '<node=' . $n['name'] . ' inbound=' . $n['inbound'] . ' outbound=' . $n['outbound'] . '>';
}
function page_rank($nodes, $limit, $d) {
  global $names, $graph, $ri, $row, $ci, $n_in, $n_out;
  $ranks = [];
  foreach ($nodes as $n) {
  $ranks[$n['name']] = 1.0;
};
  $outbounds = [];
  foreach ($nodes as $n) {
  $outbounds[$n['name']] = 1.0 * _len($n['outbound']);
};
  $i = 0;
  while ($i < $limit) {
  echo rtrim('======= Iteration ' . _str($i + 1) . ' ======='), PHP_EOL;
  foreach ($nodes as $n) {
  $sum_val = 0.0;
  foreach ($n['inbound'] as $ib) {
  $sum_val = $sum_val + $ranks[$ib] / $outbounds[$ib];
};
  $ranks[$n['name']] = (1.0 - $d) + $d * $sum_val;
};
  echo str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($ranks, 1344)))))), PHP_EOL;
  $i = $i + 1;
};
  return $ranks;
}
$names = ['A', 'B', 'C'];
$graph = [[0, 1, 1], [0, 0, 1], [1, 0, 0]];
$nodes = [];
foreach ($names as $name) {
  $nodes = _append($nodes, [$name => $name, 'inbound' => [], 'outbound' => []]);
}
$ri = 0;
while ($ri < count($graph)) {
  $row = $graph[$ri];
  $ci = 0;
  while ($ci < count($row)) {
  if ($row[$ci] == 1) {
  $n_in = $nodes[$ci];
  $n_in['inbound'] = _append($n_in['inbound'], $names[$ri]);
  $nodes[$ci] = $n_in;
  $n_out = $nodes[$ri];
  $n_out['outbound'] = _append($n_out['outbound'], $names[$ci]);
  $nodes[$ri] = $n_out;
}
  $ci = $ci + 1;
};
  $ri = $ri + 1;
}
echo rtrim('======= Nodes ======='), PHP_EOL;
foreach ($nodes as $n) {
  echo rtrim(json_encode($n, 1344)), PHP_EOL;
}
page_rank($nodes, 3, 0.85);
