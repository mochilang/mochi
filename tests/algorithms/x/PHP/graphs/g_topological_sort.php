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
  function depth_first_search($u, &$visited, $graph, $stack) {
  $visited[$u] = true;
  $i = 0;
  while ($i < count($graph[$u])) {
  $v = $graph[$u][$i];
  if (!$visited[$v]) {
  $stack = depth_first_search($v, $visited, $graph, $stack);
}
  $i = $i + 1;
};
  $stack = _append($stack, $u);
  return $stack;
};
  function topological_sort($graph) {
  $visited = [];
  $i = 0;
  while ($i < count($graph)) {
  $visited = _append($visited, false);
  $i = $i + 1;
};
  $stack = [];
  $i = 0;
  while ($i < count($graph)) {
  if (!$visited[$i]) {
  $stack = depth_first_search($i, $visited, $graph, $stack);
}
  $i = $i + 1;
};
  return $stack;
};
  function print_stack($stack, $clothes) {
  $order = 1;
  $s = $stack;
  while (count($s) > 0) {
  $idx = $s[count($s) - 1];
  $s = array_slice($s, 0, count($s) - 1);
  echo rtrim(_str($order) . ' ' . $clothes[$idx]), PHP_EOL;
  $order = $order + 1;
};
};
  function format_list($xs) {
  $res = '[';
  $i = 0;
  while ($i < count($xs)) {
  $res = $res . _str($xs[$i]);
  if ($i < count($xs) - 1) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  $res = $res . ']';
  return $res;
};
  function main() {
  $clothes = [0 => 'underwear', 1 => 'pants', 2 => 'belt', 3 => 'suit', 4 => 'shoe', 5 => 'socks', 6 => 'shirt', 7 => 'tie', 8 => 'watch'];
  $graph = [[1, 4], [2, 4], [3], [], [], [4], [2, 7], [3], []];
  $stack = topological_sort($graph);
  echo rtrim(format_list($stack)), PHP_EOL;
  print_stack($stack, $clothes);
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
