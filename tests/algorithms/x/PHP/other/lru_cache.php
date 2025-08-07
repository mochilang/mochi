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
  function new_list() {
  $nodes = [];
  $head = ['key' => 0, 'value' => 0, 'prev' => 0 - 1, 'next' => 1];
  $tail = ['key' => 0, 'value' => 0, 'prev' => 0, 'next' => 0 - 1];
  $nodes = _append($nodes, $head);
  $nodes = _append($nodes, $tail);
  return ['nodes' => $nodes, 'head' => 0, 'tail' => 1];
};
  function dll_add(&$lst, $idx) {
  $nodes = $lst['nodes'];
  $tail_idx = $lst['tail'];
  $tail_node = $nodes[$tail_idx];
  $prev_idx = $tail_node['prev'];
  $node = $nodes[$idx];
  $node['prev'] = $prev_idx;
  $node['next'] = $tail_idx;
  $nodes[$idx] = $node;
  $prev_node = $nodes[$prev_idx];
  $prev_node['next'] = $idx;
  $nodes[$prev_idx] = $prev_node;
  $tail_node['prev'] = $idx;
  $nodes[$tail_idx] = $tail_node;
  $lst['nodes'] = $nodes;
  return $lst;
};
  function dll_remove(&$lst, $idx) {
  $nodes = $lst['nodes'];
  $node = $nodes[$idx];
  $prev_idx = $node['prev'];
  $next_idx = $node['next'];
  if ($prev_idx == 0 - 1 || $next_idx == 0 - 1) {
  return $lst;
}
  $prev_node = $nodes[$prev_idx];
  $prev_node['next'] = $next_idx;
  $nodes[$prev_idx] = $prev_node;
  $next_node = $nodes[$next_idx];
  $next_node['prev'] = $prev_idx;
  $nodes[$next_idx] = $next_node;
  $node['prev'] = 0 - 1;
  $node['next'] = 0 - 1;
  $nodes[$idx] = $node;
  $lst['nodes'] = $nodes;
  return $lst;
};
  function new_cache($cap) {
  $empty_map = [];
  return ['list' => new_list(), 'capacity' => $cap, 'num_keys' => 0, 'hits' => 0, 'misses' => 0, 'cache' => $empty_map];
};
  function lru_get($c, $key) {
  $cache = $c;
  $key_str = _str($key);
  if (isset($cache['cache'][$key_str])) {
  $idx = $cache['cache'][$key_str];
  if ($idx != 0 - 1) {
  $cache['hits'] = $cache['hits'] + 1;
  $node = $cache['list']['nodes'][$idx];
  $value = $node['value'];
  $cache['list'] = dll_remove($cache['list'], $idx);
  $cache['list'] = dll_add($cache['list'], $idx);
  return ['cache' => $cache, 'value' => $value, 'ok' => true];
};
}
  $cache['misses'] = $cache['misses'] + 1;
  return ['cache' => $cache, 'value' => 0, 'ok' => false];
};
  function lru_put($c, $key, $value) {
  $cache = $c;
  $key_str = _str($key);
  if (!(isset($cache['cache'][$key_str]))) {
  if ($cache['num_keys'] >= $cache['capacity']) {
  $head_node = $cache['list']['nodes'][$cache['list']['head']];
  $first_idx = $head_node['next'];
  $first_node = $cache['list']['nodes'][$first_idx];
  $old_key = $first_node['key'];
  $cache['list'] = dll_remove($cache['list'], $first_idx);
  $mdel = $cache['cache'];
  $mdel[_str($old_key)] = 0 - 1;
  $cache['cache'] = $mdel;
  $cache['num_keys'] = $cache['num_keys'] - 1;
};
  $nodes = $cache['list']['nodes'];
  $new_node = ['key' => $key, 'value' => $value, 'prev' => 0 - 1, 'next' => 0 - 1];
  $nodes = _append($nodes, $new_node);
  $idx = count($nodes) - 1;
  $cache['list']['nodes'] = $nodes;
  $cache['list'] = dll_add($cache['list'], $idx);
  $m = $cache['cache'];
  $m[$key_str] = $idx;
  $cache['cache'] = $m;
  $cache['num_keys'] = $cache['num_keys'] + 1;
} else {
  $m = $cache['cache'];
  $idx = $m[$key_str];
  $nodes = $cache['list']['nodes'];
  $node = $nodes[$idx];
  $node['value'] = $value;
  $nodes[$idx] = $node;
  $cache['list']['nodes'] = $nodes;
  $cache['list'] = dll_remove($cache['list'], $idx);
  $cache['list'] = dll_add($cache['list'], $idx);
  $cache['cache'] = $m;
}
  return $cache;
};
  function cache_info($cache) {
  return 'CacheInfo(hits=' . _str($cache['hits']) . ', misses=' . _str($cache['misses']) . ', capacity=' . _str($cache['capacity']) . ', current size=' . _str($cache['num_keys']) . ')';
};
  function print_result($res) {
  if ($res['ok']) {
  echo rtrim(_str($res['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
};
  function main() {
  $cache = new_cache(2);
  $cache = lru_put($cache, 1, 1);
  $cache = lru_put($cache, 2, 2);
  $r1 = lru_get($cache, 1);
  $cache = $r1['cache'];
  print_result($r1);
  $cache = lru_put($cache, 3, 3);
  $r2 = lru_get($cache, 2);
  $cache = $r2['cache'];
  print_result($r2);
  $cache = lru_put($cache, 4, 4);
  $r3 = lru_get($cache, 1);
  $cache = $r3['cache'];
  print_result($r3);
  $r4 = lru_get($cache, 3);
  $cache = $r4['cache'];
  print_result($r4);
  $r5 = lru_get($cache, 4);
  $cache = $r5['cache'];
  print_result($r5);
  echo rtrim(cache_info($cache)), PHP_EOL;
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
