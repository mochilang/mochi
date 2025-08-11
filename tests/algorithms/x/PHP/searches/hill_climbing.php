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
  function score($sp) {
  return $sp['f']($sp['x'], $sp['y']);
};
  function neighbors($sp) {
  $s = $sp['step'];
  return [['x' => $sp['x'] - $s, 'y' => $sp['y'] - $s, 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'] - $s, 'y' => $sp['y'], 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'] - $s, 'y' => $sp['y'] + $s, 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'], 'y' => $sp['y'] - $s, 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'], 'y' => $sp['y'] + $s, 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'] + $s, 'y' => $sp['y'] - $s, 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'] + $s, 'y' => $sp['y'], 'step' => $s, 'f' => $sp['f']], ['x' => $sp['x'] + $s, 'y' => $sp['y'] + $s, 'step' => $s, 'f' => $sp['f']]];
};
  function equal_state($a, $b) {
  return $a['x'] == $b['x'] && $a['y'] == $b['y'];
};
  function contains_state($lst, $sp) {
  $i = 0;
  while ($i < count($lst)) {
  if (equal_state($lst[$i], $sp)) {
  return true;
}
  $i = _iadd($i, 1);
};
  return false;
};
  function hill_climbing($sp, $find_max, $max_x, $min_x, $max_y, $min_y, $max_iter) {
  $current = $sp;
  $visited = [];
  $iterations = 0;
  $solution_found = false;
  while ($solution_found == false && $iterations < $max_iter) {
  $visited = _append($visited, $current);
  $iterations = _iadd($iterations, 1);
  $current_score = score($current);
  $neighs = neighbors($current);
  $max_change = -1000000000000000000.0;
  $min_change = 1000000000000000000.0;
  $next = $current;
  $improved = false;
  $i = 0;
  while ($i < count($neighs)) {
  $n = $neighs[$i];
  $i = _iadd($i, 1);
  if (contains_state($visited, $n)) {
  continue;
}
  if ($n['x'] > $max_x || $n['x'] < $min_x || $n['y'] > $max_y || $n['y'] < $min_y) {
  continue;
}
  $change = score($n) - $current_score;
  if ($find_max) {
  if ($change > $max_change && $change > 0.0) {
  $max_change = $change;
  $next = $n;
  $improved = true;
};
} else {
  if ($change < $min_change && $change < 0.0) {
  $min_change = $change;
  $next = $n;
  $improved = true;
};
}
};
  if ($improved) {
  $current = $next;
} else {
  $solution_found = true;
}
};
  return $current;
};
  function test_f1($x, $y) {
  return $x * $x + $y * $y;
};
  function main() {
  $prob1 = ['x' => 3.0, 'y' => 4.0, 'step' => 1.0, 'f' => 'test_f1'];
  $local_min1 = hill_climbing($prob1, false, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 10000);
  echo rtrim(_str(intval(score($local_min1)))), PHP_EOL;
  $prob2 = ['x' => 12.0, 'y' => 47.0, 'step' => 1.0, 'f' => 'test_f1'];
  $local_min2 = hill_climbing($prob2, false, 100.0, 5.0, 50.0, -5.0, 10000);
  echo rtrim(_str(intval(score($local_min2)))), PHP_EOL;
  $prob3 = ['x' => 3.0, 'y' => 4.0, 'step' => 1.0, 'f' => 'test_f1'];
  $local_max = hill_climbing($prob3, true, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 1000);
  echo rtrim(_str(intval(score($local_max)))), PHP_EOL;
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
