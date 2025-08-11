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
function _iadd($a, $b) {
    if (function_exists('bcadd')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcadd($sa, $sb, 0);
    }
    return $a + $b;
}
function _isub($a, $b) {
    if (function_exists('bcsub')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcsub($sa, $sb, 0);
    }
    return $a - $b;
}
function _imul($a, $b) {
    if (function_exists('bcmul')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return bcmul($sa, $sb, 0);
    }
    return $a * $b;
}
function _idiv($a, $b) {
    return _intdiv($a, $b);
}
function _imod($a, $b) {
    if (function_exists('bcmod')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcmod($sa, $sb));
    }
    return $a % $b;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function path_cost($path, $graph) {
  global $first, $best;
  $total = 0;
  $i = 0;
  while ($i < _isub(count($path), 1)) {
  $u = $path[$i];
  $v = $path[_iadd($i, 1)];
  $total = _iadd($total, $graph[$u][$v]);
  $i = _iadd($i, 1);
};
  return $total;
};
  function generate_first_solution($graph, $start) {
  global $first, $best;
  $path = [];
  $visiting = $start;
  $total = 0;
  while (count($path) < count($graph)) {
  $path = _append($path, $visiting);
  $best_node = '';
  $best_cost = 1000000;
  foreach (array_keys($graph[$visiting]) as $n) {
  if (!(in_array($n, $path)) && $graph[$visiting][$n] < $best_cost) {
  $best_cost = $graph[$visiting][$n];
  $best_node = $n;
}
};
  if ($best_node == '') {
  break;
}
  $total = _iadd($total, $best_cost);
  $visiting = $best_node;
};
  $path = _append($path, $start);
  $total = _iadd($total, $graph[$visiting][$start]);
  return ['path' => $path, 'cost' => $total];
};
  function copy_path($path) {
  global $graph, $first, $best;
  $res = [];
  $i = 0;
  while ($i < count($path)) {
  $res = _append($res, $path[$i]);
  $i = _iadd($i, 1);
};
  return $res;
};
  function find_neighborhood($sol, $graph) {
  global $first, $best;
  $neighbors = [];
  $i = 1;
  while ($i < _isub(_len($sol['path']), 1)) {
  $j = 1;
  while ($j < _isub(_len($sol['path']), 1)) {
  if ($i != $j) {
  $new_path = copy_path($sol['path']);
  $tmp = $new_path[$i];
  $new_path[$i] = $new_path[$j];
  $new_path[$j] = $tmp;
  $cost = path_cost($new_path, $graph);
  $neighbors = _append($neighbors, ['path' => $new_path, $cost => $cost]);
}
  $j = _iadd($j, 1);
};
  $i = _iadd($i, 1);
};
  return $neighbors;
};
  function find_swap($a, $b) {
  global $graph, $first, $best;
  $i = 0;
  while ($i < count($a)) {
  if ($a[$i] != $b[$i]) {
  return ['a' => $a[$i], 'b' => $b[$i]];
}
  $i = _iadd($i, 1);
};
  return ['a' => '', 'b' => ''];
};
  function tabu_search($first, $graph, $iters, $size) {
  $solution = $first;
  $best = $first;
  $tabu = [];
  $count = 0;
  while ($count < $iters) {
  $neighborhood = find_neighborhood($solution, $graph);
  if (count($neighborhood) == 0) {
  break;
}
  $best_neighbor = $neighborhood[0];
  $best_move = find_swap($solution['path'], $best_neighbor['path']);
  $i = 1;
  while ($i < count($neighborhood)) {
  $cand = $neighborhood[$i];
  $move = find_swap($solution['path'], $cand['path']);
  $forbidden = false;
  $t = 0;
  while ($t < count($tabu)) {
  if (($tabu[$t]['a'] == $move['a'] && $tabu[$t]['b'] == $move['b']) || ($tabu[$t]['a'] == $move['b'] && $tabu[$t]['b'] == $move['a'])) {
  $forbidden = true;
}
  $t = _iadd($t, 1);
};
  if ($forbidden == false && $cand['cost'] < $best_neighbor['cost']) {
  $best_neighbor = $cand;
  $best_move = $move;
}
  $i = _iadd($i, 1);
};
  $solution = $best_neighbor;
  $tabu = _append($tabu, $best_move);
  if (count($tabu) > $size) {
  $new_tab = [];
  $j = 1;
  while ($j < count($tabu)) {
  $new_tab = _append($new_tab, $tabu[$j]);
  $j = _iadd($j, 1);
};
  $tabu = $new_tab;
}
  if ($solution['cost'] < $best['cost']) {
  $best = $solution;
}
  $count = _iadd($count, 1);
};
  return $best;
};
  $graph = ['a' => ['b' => 20, 'c' => 18, 'd' => 22, 'e' => 26], 'b' => ['a' => 20, 'c' => 10, 'd' => 11, 'e' => 12], 'c' => ['a' => 18, 'b' => 10, 'd' => 23, 'e' => 24], 'd' => ['a' => 22, 'b' => 11, 'c' => 23, 'e' => 40], 'e' => ['a' => 26, 'b' => 12, 'c' => 24, 'd' => 40]];
  $first = generate_first_solution($graph, 'a');
  $best = tabu_search($first, $graph, 4, 3);
  echo rtrim(_str($best['path'])), PHP_EOL;
  echo rtrim(_str($best['cost'])), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
