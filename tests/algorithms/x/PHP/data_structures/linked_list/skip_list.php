<?php
ini_set('memory_limit', '-1');
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
$NIL = 0 - 1;
$MAX_LEVEL = 6;
$P = 0.5;
$seed = 1;
function random() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
  $seed = ($seed * 13 + 7) % 100;
  return (floatval($seed)) / 100.0;
}
function random_level() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
  $lvl = 1;
  while (random() < $P && $lvl < $MAX_LEVEL) {
  $lvl = $lvl + 1;
};
  return $lvl;
}
function empty_forward() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
  $f = [];
  $i = 0;
  while ($i < $MAX_LEVEL) {
  $f = _append($f, $NIL);
  $i = $i + 1;
};
  return $f;
}
$node_keys = [];
$node_vals = [];
$node_forwards = [];
$level = 1;
function init() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
  $node_keys = [-1];
  $node_vals = [0];
  $node_forwards = [empty_forward()];
  $level = 1;
}
function insert($key, $value) {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
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
}
function find($key) {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
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
}
function delete($key) {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
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
}
function to_string() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
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
}
function main() {
  global $NIL, $MAX_LEVEL, $P, $seed, $node_keys, $node_vals, $node_forwards, $level;
  init();
  insert(2, 2);
  insert(4, 4);
  insert(6, 4);
  insert(4, 5);
  insert(8, 4);
  insert(9, 4);
  delete(4);
  echo rtrim(to_string()), PHP_EOL;
}
main();
