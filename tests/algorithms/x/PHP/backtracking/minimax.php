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
function _intdiv($a, $b) {
    if (function_exists('bcdiv')) {
        $sa = is_int($a) ? strval($a) : (is_string($a) ? $a : sprintf('%.0f', $a));
        $sb = is_int($b) ? strval($b) : (is_string($b) ? $b : sprintf('%.0f', $b));
        return intval(bcdiv($sa, $sb, 0));
    }
    return intdiv($a, $b);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function minimax($depth, $node_index, $is_max, $scores, $height) {
  if ($depth < 0) {
  $panic('Depth cannot be less than 0');
}
  if (count($scores) == 0) {
  $panic('Scores cannot be empty');
}
  if ($depth == $height) {
  return $scores[$node_index];
}
  if ($is_max) {
  $left = minimax($depth + 1, $node_index * 2, false, $scores, $height);
  $right = minimax($depth + 1, $node_index * 2 + 1, false, $scores, $height);
  if ($left > $right) {
  return $left;
} else {
  return $right;
};
}
  $left = minimax($depth + 1, $node_index * 2, true, $scores, $height);
  $right = minimax($depth + 1, $node_index * 2 + 1, true, $scores, $height);
  if ($left < $right) {
  return $left;
} else {
  return $right;
}
};
  function tree_height($n) {
  $h = 0;
  $v = $n;
  while ($v > 1) {
  $v = _intdiv($v, 2);
  $h = $h + 1;
};
  return $h;
};
  function main() {
  $scores = [90, 23, 6, 33, 21, 65, 123, 34423];
  $height = tree_height(count($scores));
  echo rtrim('Optimal value : ' . _str(minimax(0, 0, true, $scores, $height))), PHP_EOL;
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
