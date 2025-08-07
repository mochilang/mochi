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
  function lfu_new($cap) {
  return ['entries' => [], 'capacity' => $cap, 'hits' => 0, 'miss' => 0, 'tick' => 0];
};
  function find_entry($entries, $key) {
  $i = 0;
  while ($i < count($entries)) {
  $e = $entries[$i];
  if ($e['key'] == $key) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
};
  function lfu_get($cache, $key) {
  $idx = find_entry($cache['entries'], $key);
  if ($idx == 0 - 1) {
  $new_cache = ['entries' => $cache['entries'], 'capacity' => $cache['capacity'], 'hits' => $cache['hits'], 'miss' => $cache['miss'] + 1, 'tick' => $cache['tick']];
  return ['cache' => $new_cache, 'value' => 0, 'ok' => false];
}
  $entries = $cache['entries'];
  $e = $entries[$idx];
  $e['freq'] = $e['freq'] + 1;
  $new_tick = $cache['tick'] + 1;
  $e['order'] = $new_tick;
  $entries[$idx] = $e;
  $new_cache = ['entries' => $entries, 'capacity' => $cache['capacity'], 'hits' => $cache['hits'] + 1, 'miss' => $cache['miss'], 'tick' => $new_tick];
  return ['cache' => $new_cache, 'value' => $e['val'], 'ok' => true];
};
  function remove_lfu($entries) {
  if (count($entries) == 0) {
  return $entries;
}
  $min_idx = 0;
  $i = 1;
  while ($i < count($entries)) {
  $e = $entries[$i];
  $m = $entries[$min_idx];
  if ($e['freq'] < $m['freq'] || ($e['freq'] == $m['freq'] && $e['order'] < $m['order'])) {
  $min_idx = $i;
}
  $i = $i + 1;
};
  $res = [];
  $j = 0;
  while ($j < count($entries)) {
  if ($j != $min_idx) {
  $res = _append($res, $entries[$j]);
}
  $j = $j + 1;
};
  return $res;
};
  function lfu_put($cache, $key, $value) {
  $entries = $cache['entries'];
  $idx = find_entry($entries, $key);
  if ($idx != 0 - 1) {
  $e = $entries[$idx];
  $e['val'] = $value;
  $e['freq'] = $e['freq'] + 1;
  $new_tick = $cache['tick'] + 1;
  $e['order'] = $new_tick;
  $entries[$idx] = $e;
  return ['entries' => $entries, 'capacity' => $cache['capacity'], 'hits' => $cache['hits'], 'miss' => $cache['miss'], 'tick' => $new_tick];
}
  if (count($entries) >= $cache['capacity']) {
  $entries = remove_lfu($entries);
}
  $new_tick = $cache['tick'] + 1;
  $new_entry = ['key' => $key, 'val' => $value, 'freq' => 1, 'order' => $new_tick];
  $entries = _append($entries, $new_entry);
  return ['entries' => $entries, 'capacity' => $cache['capacity'], 'hits' => $cache['hits'], 'miss' => $cache['miss'], 'tick' => $new_tick];
};
  function cache_info($cache) {
  return 'CacheInfo(hits=' . _str($cache['hits']) . ', misses=' . _str($cache['miss']) . ', capacity=' . _str($cache['capacity']) . ', current_size=' . _str(_len($cache['entries'])) . ')';
};
  function main() {
  $cache = lfu_new(2);
  $cache = lfu_put($cache, 1, 1);
  $cache = lfu_put($cache, 2, 2);
  $r = lfu_get($cache, 1);
  $cache = $r['cache'];
  if ($r['ok']) {
  echo rtrim(_str($r['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
  $cache = lfu_put($cache, 3, 3);
  $r = lfu_get($cache, 2);
  $cache = $r['cache'];
  if ($r['ok']) {
  echo rtrim(_str($r['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
  $cache = lfu_put($cache, 4, 4);
  $r = lfu_get($cache, 1);
  $cache = $r['cache'];
  if ($r['ok']) {
  echo rtrim(_str($r['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
  $r = lfu_get($cache, 3);
  $cache = $r['cache'];
  if ($r['ok']) {
  echo rtrim(_str($r['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
  $r = lfu_get($cache, 4);
  $cache = $r['cache'];
  if ($r['ok']) {
  echo rtrim(_str($r['value'])), PHP_EOL;
} else {
  echo rtrim('None'), PHP_EOL;
}
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
