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
  $seed = 1;
  function mochi_rand() {
  global $seed;
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random() {
  global $seed;
  return (1.0 * mochi_rand()) / 2147483648.0;
};
  function get_nodes($trans) {
  global $seed;
  $seen = [];
  foreach ($trans as $t) {
  $seen[$t['src']] = true;
  $seen[$t['dst']] = true;
};
  $nodes = [];
  foreach (array_keys($seen) as $k) {
  $nodes = _append($nodes, $k);
};
  return $nodes;
};
  function transition($current, $trans) {
  global $seed;
  $current_probability = 0.0;
  $random_value = random();
  foreach ($trans as $t) {
  if ($t['src'] == $current) {
  $current_probability = $current_probability + $t['prob'];
  if ($current_probability > $random_value) {
  return $t['dst'];
};
}
};
  return '';
};
  function get_transitions($start, $trans, $steps) {
  global $seed;
  $visited = [];
  foreach (get_nodes($trans) as $node) {
  $one = 1;
  $visited[$node] = $one;
};
  $node = $start;
  $i = 0;
  while ($i < $steps) {
  $node = transition($node, $trans);
  $count = $visited[$node];
  $count = $count + 1;
  $visited[$node] = $count;
  $i = $i + 1;
};
  return $visited;
};
  function main() {
  global $seed;
  $transitions = [['src' => 'a', 'dst' => 'a', 'prob' => 0.9], ['src' => 'a', 'dst' => 'b', 'prob' => 0.075], ['src' => 'a', 'dst' => 'c', 'prob' => 0.025], ['src' => 'b', 'dst' => 'a', 'prob' => 0.15], ['src' => 'b', 'dst' => 'b', 'prob' => 0.8], ['src' => 'b', 'dst' => 'c', 'prob' => 0.05], ['src' => 'c', 'dst' => 'a', 'prob' => 0.25], ['src' => 'c', 'dst' => 'b', 'prob' => 0.25], ['src' => 'c', 'dst' => 'c', 'prob' => 0.5]];
  $result = get_transitions('a', $transitions, 5000);
  echo rtrim(_str($result['a']) . ' ' . _str($result['b']) . ' ' . _str($result['c'])), PHP_EOL;
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
