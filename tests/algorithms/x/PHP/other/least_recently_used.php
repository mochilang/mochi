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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function new_cache($n) {
  global $lru, $r;
  if ($n < 0) {
  _panic('n should be an integer greater than 0.');
}
  $cap = ($n == 0 ? 2147483647 : $n);
  return ['max_capacity' => $cap, 'store' => []];
};
  function remove_element($xs, $x) {
  global $lru, $r;
  $res = [];
  $removed = false;
  $i = 0;
  while ($i < count($xs)) {
  $v = $xs[$i];
  if ($removed == false && $v == $x) {
  $removed = true;
} else {
  $res = array_merge($res, [$v]);
}
  $i = $i + 1;
};
  return $res;
};
  function refer($cache, $x) {
  global $lru, $r;
  $store = $cache['store'];
  $exists = false;
  $i = 0;
  while ($i < count($store)) {
  if ($store[$i] == $x) {
  $exists = true;
}
  $i = $i + 1;
};
  if ($exists) {
  $store = remove_element($store, $x);
} else {
  if (count($store) == $cache['max_capacity']) {
  $new_store = [];
  $j = 0;
  while ($j < count($store) - 1) {
  $new_store = array_merge($new_store, [$store[$j]]);
  $j = $j + 1;
};
  $store = $new_store;
};
}
  $store = array_merge([$x], $store);
  return ['max_capacity' => $cache['max_capacity'], 'store' => $store];
};
  function display($cache) {
  global $lru, $r;
  $i = 0;
  while ($i < _len($cache['store'])) {
  echo rtrim(json_encode($cache['store'][$i], 1344)), PHP_EOL;
  $i = $i + 1;
};
};
  function repr_item($s) {
  global $lru, $r;
  $all_digits = true;
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch < '0' || $ch > '9') {
  $all_digits = false;
}
  $i = $i + 1;
};
  if ($all_digits) {
  return $s;
}
  return '\'' . $s . '\'';
};
  function cache_repr($cache) {
  global $lru, $r;
  $res = 'LRUCache(' . _str($cache['max_capacity']) . ') => [';
  $i = 0;
  while ($i < _len($cache['store'])) {
  $res = $res . repr_item($cache['store'][$i]);
  if ($i < _len($cache['store']) - 1) {
  $res = $res . ', ';
}
  $i = $i + 1;
};
  $res = $res . ']';
  return $res;
};
  $lru = new_cache(4);
  $lru = refer($lru, 'A');
  $lru = refer($lru, '2');
  $lru = refer($lru, '3');
  $lru = refer($lru, 'A');
  $lru = refer($lru, '4');
  $lru = refer($lru, '5');
  $r = cache_repr($lru);
  echo rtrim($r), PHP_EOL;
  if ($r != 'LRUCache(4) => [5, 4, \'A\', 3]') {
  _panic('Assertion error');
}
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
