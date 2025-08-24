<?php
error_reporting(E_ALL & ~E_DEPRECATED);
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
  $NIL = 0 - 1;
  $MAX_LEVEL = 6;
  $P = 0.5;
  $seed = 1;
  function random() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $seed = ($seed * 13 + 7) % 100;
  return (floatval($seed)) / 100.0;
};
  function random_level() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $lvl = 1;
  while (random() < $P && $lvl < $MAX_LEVEL) {
  $lvl = $lvl + 1;
};
  return $lvl;
};
  function empty_forward() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $f = [];
  $i = 0;
  while ($i < $MAX_LEVEL) {
  $f = _append($f, $NIL);
  $i = $i + 1;
};
  return $f;
};
  $node_keys = [];
  $node_vals = [];
  $node_forwards = [];
  $level = 1;
  function init() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $node_keys = [-1];
  $node_vals = [0];
  $node_forwards = [empty_forward()];
  $level = 1;
};
  function insert($key, $value) {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $update = [];
  $i = 0;
  while ($i < $MAX_LEVEL) {
  $update = _append($update, 0);
  $i = $i + 1;
};
  $x = 0;
  $i = $level - 1;
  while ($i >= 0) {
  while ($node_forwards[$x][$i] != $NIL && $node_keys[$node_forwards[$x][$i]] < $key) {
  $x = $node_forwards[$x][$i];
};
  $update[$i] = $x;
  $i = $i - 1;
};
  $x = $node_forwards[$x][0];
  if ($x != $NIL && $node_keys[$x] == $key) {
  $node_vals[$x] = $value;
  return;
}
  $lvl = random_level();
  if ($lvl > $level) {
  $j = $level;
  while ($j < $lvl) {
  $update[$j] = 0;
  $j = $j + 1;
};
  $level = $lvl;
}
  $node_keys = _append($node_keys, $key);
  $node_vals = _append($node_vals, $value);
  $forwards = empty_forward();
  $idx = count($node_keys) - 1;
  $i = 0;
  while ($i < $lvl) {
  $forwards[$i] = $node_forwards[$update[$i]][$i];
  $node_forwards[$update[$i]][$i] = $idx;
  $i = $i + 1;
};
  $node_forwards = _append($node_forwards, $forwards);
};
  function find($key) {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $x = 0;
  $i = $level - 1;
  while ($i >= 0) {
  while ($node_forwards[$x][$i] != $NIL && $node_keys[$node_forwards[$x][$i]] < $key) {
  $x = $node_forwards[$x][$i];
};
  $i = $i - 1;
};
  $x = $node_forwards[$x][0];
  if ($x != $NIL && $node_keys[$x] == $key) {
  return $node_vals[$x];
}
  return -1;
};
  function delete($key) {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $update = [];
  $i = 0;
  while ($i < $MAX_LEVEL) {
  $update = _append($update, 0);
  $i = $i + 1;
};
  $x = 0;
  $i = $level - 1;
  while ($i >= 0) {
  while ($node_forwards[$x][$i] != $NIL && $node_keys[$node_forwards[$x][$i]] < $key) {
  $x = $node_forwards[$x][$i];
};
  $update[$i] = $x;
  $i = $i - 1;
};
  $x = $node_forwards[$x][0];
  if ($x == $NIL || $node_keys[$x] != $key) {
  return;
}
  $i = 0;
  while ($i < $level) {
  if ($node_forwards[$update[$i]][$i] == $x) {
  $node_forwards[$update[$i]][$i] = $node_forwards[$x][$i];
}
  $i = $i + 1;
};
  while ($level > 1 && $node_forwards[0][$level - 1] == $NIL) {
  $level = $level - 1;
};
};
  function to_string() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  $s = '';
  $x = $node_forwards[0][0];
  while ($x != $NIL) {
  if ($s != '') {
  $s = $s . ' -> ';
}
  $s = $s . _str($node_keys[$x]) . ':' . _str($node_vals[$x]);
  $x = $node_forwards[$x][0];
};
  return $s;
};
  function main() {
  global $MAX_LEVEL, $NIL, $P, $level, $node_forwards, $node_keys, $node_vals, $seed;
  init();
  insert(2, 2);
  insert(4, 4);
  insert(6, 4);
  insert(4, 5);
  insert(8, 4);
  insert(9, 4);
  delete(4);
  echo rtrim(to_string()), PHP_EOL;
};
  main();
$__end = _now();
$__end_mem = memory_get_peak_usage(true);
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;
