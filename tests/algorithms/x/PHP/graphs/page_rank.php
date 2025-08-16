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
function _len($x) {
    if ($x === null) { return 0; }
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
$__start_mem = memory_get_usage();
$__start = _now();
  function node_to_string($n) {
  global $ci, $graph, $n_in, $n_out, $names, $nodes, $ri, $row;
  return '<node=' . $n['name'] . ' inbound=' . $n['inbound'] . ' outbound=' . $n['outbound'] . '>';
};
  function page_rank($nodes, $limit, $d) {
  global $ci, $graph, $n_in, $n_out, $names, $ri, $row;
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
};
  $names = ['A', 'B', 'C'];
  $graph = [[0, 1, 1], [0, 0, 1], [1, 0, 0]];
  $nodes = [];
  foreach ($names as $name) {
  $nodes = _append($nodes, ['name' => $name, 'inbound' => [], 'outbound' => []]);
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
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
