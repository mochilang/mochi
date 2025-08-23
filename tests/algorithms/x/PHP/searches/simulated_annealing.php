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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function score($p, $f) {
  global $seed;
  return $f($p['x'], $p['y']);
};
  function get_neighbors($p) {
  global $seed;
  $s = $p['step'];
  $ns = [];
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] - $s, 'y' => $p['y'] - $s]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] - $s, 'y' => $p['y']]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] - $s, 'y' => $p['y'] + $s]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'], 'y' => $p['y'] - $s]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'], 'y' => $p['y'] + $s]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] + $s, 'y' => $p['y'] - $s]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] + $s, 'y' => $p['y']]);
  $ns = _append($ns, ['step' => $s, 'x' => $p['x'] + $s, 'y' => $p['y'] + $s]);
  return $ns;
};
  function remove_at($lst, $idx) {
  global $seed;
  $res = [];
  $i = 0;
  while ($i < count($lst)) {
  if ($i != $idx) {
  $res = _append($res, $lst[$i]);
}
  $i = $i + 1;
};
  return $res;
};
  $seed = 1;
  function mochi_rand() {
  global $seed;
  $_t = _now();
  $seed = ($seed * 1103515245 + 12345) % 2147483648;
  return $seed;
};
  function random_float() {
  global $seed;
  return (floatval(mochi_rand())) / 2147483648.0;
};
  function randint($low, $high) {
  global $seed;
  return (fmod(mochi_rand(), ($high - $low + 1))) + $low;
};
  function expApprox($x) {
  global $seed;
  $y = $x;
  $is_neg = false;
  if ($x < 0.0) {
  $is_neg = true;
  $y = -$x;
}
  $term = 1.0;
  $sum = 1.0;
  $n = 1;
  while ($n < 30) {
  $term = $term * $y / (floatval($n));
  $sum = $sum + $term;
  $n = $n + 1;
};
  if ($is_neg) {
  return 1.0 / $sum;
}
  return $sum;
};
  function simulated_annealing($search_prob, $f, $find_max, $max_x, $min_x, $max_y, $min_y, $start_temp, $rate_of_decrease, $threshold_temp) {
  global $seed;
  $search_end = false;
  $current_state = $search_prob;
  $current_temp = $start_temp;
  $best_state = $current_state;
  while (!$search_end) {
  $current_score = score($current_state, $f);
  if (score($best_state, $f) < $current_score) {
  $best_state = $current_state;
}
  $next_state = $current_state;
  $found_next = false;
  $neighbors = get_neighbors($current_state);
  while (!$found_next && count($neighbors) > 0) {
  $idx = randint(0, count($neighbors) - 1);
  $picked_neighbor = $neighbors[$idx];
  $neighbors = remove_at($neighbors, $idx);
  if ($picked_neighbor['x'] > $max_x || $picked_neighbor['x'] < $min_x || $picked_neighbor['y'] > $max_y || $picked_neighbor['y'] < $min_y) {
  continue;
}
  $change = score($picked_neighbor, $f) - $current_score;
  if (!$find_max) {
  $change = -$change;
}
  if ($change > 0.0) {
  $next_state = $picked_neighbor;
  $found_next = true;
} else {
  $probability = expApprox($change / $current_temp);
  if (random_float() < $probability) {
  $next_state = $picked_neighbor;
  $found_next = true;
};
}
};
  $current_temp = $current_temp - ($current_temp * $rate_of_decrease);
  if ($current_temp < $threshold_temp || (!$found_next)) {
  $search_end = true;
} else {
  $current_state = $next_state;
}
};
  return $best_state;
};
  function test_f1($x, $y) {
  global $seed;
  return $x * $x + $y * $y;
};
  function test_f2($x, $y) {
  global $seed;
  return (3.0 * $x * $x) - (6.0 * $y);
};
  function main() {
  global $seed;
  $prob1 = ['step' => 1.0, 'x' => 12.0, 'y' => 47.0];
  $min_state = simulated_annealing($prob1, 'test_f1', false, 100.0, 5.0, 50.0, -5.0, 100.0, 0.01, 1.0);
  echo rtrim('min1') . " " . rtrim(json_encode(test_f1($min_state['x'], $min_state['y']), 1344)), PHP_EOL;
  $prob2 = ['step' => 1.0, 'x' => 12.0, 'y' => 47.0];
  $max_state = simulated_annealing($prob2, 'test_f1', true, 100.0, 5.0, 50.0, -5.0, 100.0, 0.01, 1.0);
  echo rtrim('max1') . " " . rtrim(json_encode(test_f1($max_state['x'], $max_state['y']), 1344)), PHP_EOL;
  $prob3 = ['step' => 1.0, 'x' => 3.0, 'y' => 4.0];
  $min_state2 = simulated_annealing($prob3, 'test_f2', false, 1000.0, -1000.0, 1000.0, -1000.0, 100.0, 0.01, 1.0);
  echo rtrim('min2') . " " . rtrim(json_encode(test_f2($min_state2['x'], $min_state2['y']), 1344)), PHP_EOL;
  $prob4 = ['step' => 1.0, 'x' => 3.0, 'y' => 4.0];
  $max_state2 = simulated_annealing($prob4, 'test_f2', true, 1000.0, -1000.0, 1000.0, -1000.0, 100.0, 0.01, 1.0);
  echo rtrim('max2') . " " . rtrim(json_encode(test_f2($max_state2['x'], $max_state2['y']), 1344)), PHP_EOL;
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
